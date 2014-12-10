open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let deriver = "ord"
let raise_errorf = Ppx_deriving.raise_errorf

let parse_options options =
  options |> List.iter (fun (name, expr) ->
    match name with
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name)

let attr_compare attrs =
  Ppx_deriving.(attrs |> attr ~deriver "compare" |> Arg.(get_attr ~deriver expr))

let argn kind =
  Printf.sprintf (match kind with `lhs -> "lhs%d" | `rhs -> "rhs%d")

let compare_reduce acc expr =
  [%expr match [%e expr] with (-1|1) as x -> x | _ -> [%e acc]]

let wildcard_case int_cases =
  Exp.case [%pat? _] [%expr
    let to_int = [%e Exp.function_ int_cases] in
    (fun (a:int) b -> Pervasives.compare a b) (to_int lhs) (to_int rhs)]

let pattn side typs =
  List.mapi (fun i _ -> pvar (argn side i)) typs

let rec exprsn typs =
  typs |> List.mapi (fun i typ ->
    app (expr_of_typ typ) [evar (argn `lhs i); evar (argn `rhs i)])

and expr_of_typ typ =
  match attr_compare typ.ptyp_attributes with
  | Some fn -> fn
  | None ->
    match typ with
    | [%type: _] -> [%expr fun _ _ -> 0]
    | [%type: int] | [%type: int32] | [%type: Int32.t]
    | [%type: int64] | [%type: Int64.t] | [%type: nativeint] | [%type: Nativeint.t]
    | [%type: float] | [%type: bool] | [%type: char] | [%type: string] | [%type: bytes] ->
      [%expr (fun (a:[%t typ]) b -> Pervasives.compare a b)]
    | [%type: [%t? typ] ref]   -> [%expr fun a b -> [%e expr_of_typ typ] !a !b]
    | [%type: [%t? typ] list]  ->
      [%expr
        let rec loop x y =
          match x, y with
          | [], [] -> 0
          | [], _ -> -1
          | _, [] -> 1
          | a :: x, b :: y ->
            [%e compare_reduce [%expr loop x y] [%expr [%e expr_of_typ typ] a b]]
        in loop]
    | [%type: [%t? typ] array] ->
      [%expr fun x y ->
        let rec loop i =
          if i = Array.length x then 0
          else [%e compare_reduce [%expr loop (i + 1)]
                                  [%expr [%e expr_of_typ typ] x.(i) y.(i)]]
        in
        [%e compare_reduce [%expr loop 0]
                           [%expr Pervasives.compare (Array.length x) (Array.length y)]]]
    | [%type: [%t? typ] option] ->
      [%expr fun x y ->
        match x, y with
        | None, None -> 0
        | Some a, Some b -> [%e expr_of_typ typ] a b
        | None, Some _ -> -1
        | Some _, None -> 1]
    | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
      let compare_fn = Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "compare") lid)) in
      app compare_fn (List.map expr_of_typ args)
    | { ptyp_desc = Ptyp_tuple typs } ->
      [%expr fun [%p ptuple (pattn `lhs typs)] [%p ptuple (pattn `rhs typs)] ->
        [%e exprsn typs |> List.rev |> List.fold_left compare_reduce [%expr 0]]]
    | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
      let cases =
        fields |> List.map (fun field ->
          let pdup f = ptuple [f "lhs"; f "rhs"] in
          match field with
          | Rtag (label, _, true (*empty*), []) ->
            Exp.case (pdup (fun _ -> Pat.variant label None)) [%expr 0]
          | Rtag (label, _, false, [typ]) ->
            Exp.case (pdup (fun var -> Pat.variant label (Some (pvar var))))
                     (app (expr_of_typ typ) [evar "lhs"; evar "rhs"])
          | Rinherit ({ ptyp_desc = Ptyp_constr (tname, _) } as typ) ->
            Exp.case (pdup (fun var -> Pat.alias (Pat.type_ tname) (mknoloc var)))
                     (app (expr_of_typ typ) [evar "lhs"; evar "rhs"])
          | _ ->
            raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                         deriver (Ppx_deriving.string_of_core_type typ))
      in
      let int_cases =
        fields |> List.mapi (fun i field ->
          match field with
          | Rtag (label, _, true (*empty*), []) ->
            Exp.case (Pat.variant label None) (int i)
          | Rtag (label, _, false, [typ]) ->
            Exp.case (Pat.variant label (Some [%pat? _])) (int i)
          | Rinherit { ptyp_desc = Ptyp_constr (tname, []) } ->
            Exp.case (Pat.type_ tname) (int i)
          | _ -> assert false)
      in
      [%expr fun lhs rhs ->
        [%e Exp.match_ [%expr lhs, rhs] (cases @ [wildcard_case int_cases])]]
    | { ptyp_desc = Ptyp_var name } -> evar ("poly_"^name)
    | { ptyp_desc = Ptyp_alias (typ, _) } -> expr_of_typ typ
    | { ptyp_loc } ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                   deriver (Ppx_deriving.string_of_core_type typ)

let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  parse_options options;
  let comparator =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest -> expr_of_typ manifest
    | Ptype_variant constrs, _ ->
      let int_cases =
          constrs |> List.mapi (fun i { pcd_name = { txt = name }; pcd_args } ->
            match pcd_args with
            | [] -> Exp.case (pconstr name []) (int i)
            | _  -> Exp.case (pconstr name (List.map (fun _ -> [%pat? _]) pcd_args)) (int i))
      and cases =
        constrs |> List.map (fun { pcd_name = { txt = name }; pcd_args = typs } ->
          exprsn typs |> List.rev |> List.fold_left compare_reduce [%expr 0] |>
          Exp.case (ptuple [pconstr name (pattn `lhs typs);
                            pconstr name (pattn `rhs typs)]))
      in
      [%expr fun lhs rhs ->
        [%e Exp.match_ [%expr lhs, rhs] (cases @ [wildcard_case int_cases])]]
    | Ptype_record labels, _ ->
      let exprs =
        labels |> List.map (fun { pld_name = { txt = name }; pld_type } ->
          let field obj = Exp.field obj (mknoloc (Lident name)) in
          app (expr_of_typ pld_type) [field (evar "lhs"); field (evar "rhs")])
      in
      [%expr fun lhs rhs -> [%e List.fold_left compare_reduce [%expr 0] exprs]]
    | Ptype_abstract, None ->
      raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    | Ptype_open, _ ->
      raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  [Vb.mk (pvar (Ppx_deriving.mangle_type_decl (`Prefix "compare") type_decl))
         (polymorphize comparator)]

let sig_of_type ~options ~path type_decl =
  parse_options options;
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let polymorphize = Ppx_deriving.poly_arrow_of_type_decl
          (fun var -> [%type: [%t var] -> [%t var] -> int]) type_decl in
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "compare") type_decl))
              (polymorphize [%type: [%t typ] -> [%t typ] -> int]))]

let () =
  Ppx_deriving.(register deriver {
    core_type = Some expr_of_typ;
    structure = (fun ~options ~path type_decls ->
      [Str.value Recursive (List.concat (List.map (str_of_type ~options ~path) type_decls))]);
    signature = (fun ~options ~path type_decls ->
      List.concat (List.map (sig_of_type ~options ~path) type_decls));
  })

