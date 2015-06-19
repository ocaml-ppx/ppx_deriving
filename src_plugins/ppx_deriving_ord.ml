open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

module StringSet = Ppx_deriving.StringSet

let deriver = "ord"
let raise_errorf = Ppx_deriving.raise_errorf

type ord_options =
  {
    allow_std_type_shadowing: bool;
  }

let default_ord_options =
  {
    allow_std_type_shadowing = false;
  }

let parse_options options =
  let option_parser acc (name, expr) =
    match name with
    | "allow_std_type_shadowing" -> { allow_std_type_shadowing = true }
    | _ ->
      raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name in
  List.fold_left option_parser default_ord_options options

let attr_compare attrs =
  Ppx_deriving.(attrs |> attr ~deriver "compare" |> Arg.(get_attr ~deriver expr))

let argn kind =
  Printf.sprintf (match kind with `lhs -> "lhs%d" | `rhs -> "rhs%d")

let compare_reduce acc expr =
  [%expr match [%e expr] with 0 -> [%e acc] | x -> x]

let reduce_compare = function
  | [] -> [%expr 0]
  | x :: xs -> List.fold_left compare_reduce x xs

let wildcard_case int_cases =
  Exp.case [%pat? _] [%expr
    let to_int = [%e Exp.function_ int_cases] in
    Pervasives.compare (to_int lhs) (to_int rhs)]

(* deactivate warning 4 in code that uses [wildcard_case] *)
let warning_minus_4 =
  Ppx_deriving.attr_warning [%expr "-4"]

let pattn side typs =
  List.mapi (fun i _ -> pvar (argn side i)) typs

let rec exprsn group_def typs =
  typs |> List.mapi (fun i typ ->
    app (expr_of_typ group_def typ) [evar (argn `lhs i); evar (argn `rhs i)])

and expr_of_typ group_def typ =
  let rec aux typ =
    match attr_compare typ.ptyp_attributes with
    | Some fn -> fn
    | None ->
      match typ with
      | { ptyp_desc = Ptyp_constr ({ txt = (Lident id as lid) }, args) } when
            StringSet.mem id group_def ->
        let compare_fn = Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "compare") lid)) in
        app compare_fn (List.map aux args)
      | [%type: _] | [%type: unit] -> [%expr fun _ _ -> 0]
      | [%type: int] | [%type: int32] | [%type: Int32.t]
      | [%type: int64] | [%type: Int64.t] | [%type: nativeint] | [%type: Nativeint.t]
      | [%type: float] | [%type: bool] | [%type: char] | [%type: string] | [%type: String.t] |
        [%type: bytes] ->
        [%expr Pervasives.compare]
      | [%type: [%t? typ] ref]   -> [%expr fun a b -> [%e aux typ] !a !b]
      | [%type: [%t? typ] list]  ->
        [%expr
          let rec loop x y =
            match x, y with
            | [], [] -> 0
            | [], _ -> -1
            | _, [] -> 1
            | a :: x, b :: y ->
              [%e compare_reduce [%expr loop x y] [%expr [%e aux typ] a b]]
          in (fun x y -> loop x y)]
      | [%type: [%t? typ] array] ->
        [%expr fun x y ->
          let rec loop i =
            if i = Array.length x then 0
            else [%e compare_reduce [%expr loop (i + 1)]
                                    [%expr [%e aux typ] x.(i) y.(i)]]
          in
          [%e compare_reduce [%expr loop 0]
                             [%expr Pervasives.compare (Array.length x) (Array.length y)]]]
      | [%type: [%t? typ] option] ->
        [%expr fun x y ->
          match x, y with
          | None, None -> 0
          | Some a, Some b -> [%e aux typ] a b
          | None, Some _ -> -1
          | Some _, None -> 1]
      | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
        let compare_fn = Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "compare") lid)) in
        app compare_fn (List.map aux args)
      | { ptyp_desc = Ptyp_tuple typs } ->
        [%expr fun [%p ptuple (pattn `lhs typs)] [%p ptuple (pattn `rhs typs)] ->
          [%e exprsn group_def typs |> List.rev |> reduce_compare]]
      | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
        let cases =
          fields |> List.map (fun field ->
            let pdup f = ptuple [f "lhs"; f "rhs"] in
            match field with
            | Rtag (label, _, true (*empty*), []) ->
              Exp.case (pdup (fun _ -> Pat.variant label None)) [%expr 0]
            | Rtag (label, _, false, [typ]) ->
              Exp.case (pdup (fun var -> Pat.variant label (Some (pvar var))))
                       (app (aux typ) [evar "lhs"; evar "rhs"])
            | Rinherit ({ ptyp_desc = Ptyp_constr (tname, _) } as typ) ->
              Exp.case (pdup (fun var -> Pat.alias (Pat.type_ tname) (mknoloc var)))
                       (app (aux typ) [evar "lhs"; evar "rhs"])
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
          [%e Exp.match_ ~attrs:[warning_minus_4] [%expr lhs, rhs] (cases @ [wildcard_case int_cases])]]
      | { ptyp_desc = Ptyp_var name } -> evar ("poly_"^name)
      | { ptyp_desc = Ptyp_alias (typ, _) } -> aux typ
      | { ptyp_loc } ->
        raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                     deriver (Ppx_deriving.string_of_core_type typ) in
  aux typ

let core_type_of_decl ~options ~path type_decl =
  ignore (parse_options options);
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let polymorphize = Ppx_deriving.poly_arrow_of_type_decl
          (fun var -> [%type: [%t var] -> [%t var] -> int]) type_decl in
  (polymorphize [%type: [%t typ] -> [%t typ] -> int])

let sig_of_type ~options ~path type_decl =
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "compare") type_decl))
             (core_type_of_decl ~options ~path type_decl))]

let str_of_type ~options ~path group_def ({ ptype_loc = loc } as type_decl) =
  ignore (parse_options options);
  let comparator =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest -> expr_of_typ group_def manifest
    | Ptype_variant constrs, _ ->
      let int_cases =
          constrs |> List.mapi (fun i { pcd_name = { txt = name }; pcd_args } ->
            match pcd_args with
            | [] -> Exp.case (pconstr name []) (int i)
            | _  -> Exp.case (pconstr name (List.map (fun _ -> [%pat? _]) pcd_args)) (int i))
      and cases =
        constrs |> List.map (fun { pcd_name = { txt = name }; pcd_args = typs } ->
          exprsn group_def typs |> List.rev |> reduce_compare |>
          Exp.case (ptuple [pconstr name (pattn `lhs typs);
                            pconstr name (pattn `rhs typs)])) in
      (* if the type as only one constructor, generating wildcard yield a warning. *)
      let wildcard = match constrs with
        | [] | [_] -> []
        | _ :: _ :: _ -> [wildcard_case int_cases] in
      [%expr fun lhs rhs ->
        [%e Exp.match_ ~attrs:[warning_minus_4] [%expr lhs, rhs] (cases @ wildcard)]]
    | Ptype_record labels, _ ->
      let exprs =
        labels |> List.map (fun { pld_name = { txt = name }; pld_type; pld_attributes } ->
          let attrs = pld_attributes @ pld_type.ptyp_attributes in
          let pld_type = {pld_type with ptyp_attributes=attrs} in
          let field obj = Exp.field obj (mknoloc (Lident name)) in
          app (expr_of_typ group_def pld_type) [field (evar "lhs"); field (evar "rhs")])
      in
      [%expr fun lhs rhs -> [%e reduce_compare exprs]]
    | Ptype_abstract, None ->
      raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    | Ptype_open, _ ->
      raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let out_type =
    Ppx_deriving.strong_type_of_type @@
      core_type_of_decl ~options ~path type_decl in
  let out_var =
    pvar (Ppx_deriving.mangle_type_decl (`Prefix "compare") type_decl) in
  [Vb.mk (Pat.constraint_ out_var out_type)
         (polymorphize comparator)]

let type_decl_str ~options ~path type_decls =
  let opts = parse_options options in
  let typename_set =
    Ppx_deriving.extract_typename_of_type_group
      deriver
      ~allow_shadowing:opts.allow_std_type_shadowing
      type_decls in
  let here_loc = (List.hd type_decls).ptype_loc in
  if StringSet.mem "int" typename_set then
    raise_errorf
      ~loc:here_loc
      "%s can't derivate types when shadowing int (even with option)" deriver;
  let code =
    List.map (str_of_type ~options ~path typename_set) type_decls in
  [Str.value Recursive (List.concat code)]

let () =
  Ppx_deriving.(register (create deriver
    ~core_type: (expr_of_typ StringSet.empty)
    ~type_decl_str: type_decl_str
    ~type_decl_sig: (fun ~options ~path type_decls ->
      List.concat (List.map (sig_of_type ~options ~path) type_decls))
    ()
  ))
