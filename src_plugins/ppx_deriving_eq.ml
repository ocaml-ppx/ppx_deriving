open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let prefix = "eq"
let raise_errorf = Ppx_deriving.raise_errorf

let argn kind =
  Printf.sprintf (match kind with `lhs -> "lhs%d" | `rhs -> "rhs%d")

let pattn side typs =
  List.mapi (fun i _ -> pvar (argn side i)) typs

let rec exprsn typs =
  typs |> List.mapi (fun i typ ->
    app (expr_of_typ typ) [evar (argn `lhs i); evar (argn `rhs i)])

and expr_of_typ typ =
  match Ppx_deriving.attr ~prefix "equal" typ.ptyp_attributes with
  | Some (_, PStr [{ pstr_desc = Pstr_eval (equal, _) }]) -> equal
  | Some ({ loc }, _) -> raise_errorf ~loc "Invalid [@deriving.%s.equal] syntax" prefix
  | None ->
    match typ with
    | [%type: int] | [%type: int32] | [%type: Int32.t]
    | [%type: int64] | [%type: Int64.t] | [%type: nativeint] | [%type: Nativeint.t]
    | [%type: float] | [%type: bool] | [%type: char] | [%type: string] | [%type: bytes] ->
      [%expr (fun (a:[%t typ]) b -> a = b)]
    | [%type: [%t? typ] ref]   -> [%expr fun a b -> [%e expr_of_typ typ] !a !b]
    | [%type: [%t? typ] list]  ->
      [%expr
        let rec loop x y =
          match x, y with
          | [], [] -> true
          | a :: x, b :: y -> [%e expr_of_typ typ] a b && loop x y
          | _ -> false
        in loop]
    | [%type: [%t? typ] array] ->
      [%expr fun x y ->
        let rec loop i =
          (i = Array.length x || [%e expr_of_typ typ] x.(i) y.(i)) && loop (i + 1)
        in Array.length x = Array.length y && loop 0]
    | [%type: [%t? typ] option] ->
      [%expr fun x y ->
        match x, y with
        | None, None -> true
        | Some a, Some b -> [%e expr_of_typ typ] a b
        | _ -> false]
    | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
      let equal_fn = Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "equal") lid)) in
      app equal_fn (List.map expr_of_typ args)
    | { ptyp_desc = Ptyp_tuple typs } ->
      [%expr fun [%p ptuple (pattn `lhs typs)] [%p ptuple (pattn `rhs typs)] ->
        [%e exprsn typs |> Ppx_deriving.(fold_exprs (binop_reduce [%expr (&&)]))]]
    | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
      let cases =
        (fields |> List.map (fun field ->
          let pdup f = ptuple [f "lhs"; f "rhs"] in
          match field with
          | Rtag (label, _, true (*empty*), []) ->
            Exp.case (pdup (fun _ -> Pat.variant label None)) [%expr true]
          | Rtag (label, _, false, [typ]) ->
            Exp.case (pdup (fun var -> Pat.variant label (Some (pvar var))))
                     (app (expr_of_typ typ) [evar "lhs"; evar "rhs"])
          | Rinherit ({ ptyp_desc = Ptyp_constr (tname, []) } as typ) ->
            Exp.case (pdup (fun var -> Pat.alias (Pat.type_ tname) (mknoloc var)))
                     (app (expr_of_typ typ) [evar "lhs"; evar "rhs"])
          | _ ->
            raise_errorf ~loc:ptyp_loc "Cannot derive eq for %s"
                         (Ppx_deriving.string_of_core_type typ))) @
        [Exp.case (pvar "_") [%expr false]]
      in
      [%expr fun lhs rhs -> [%e Exp.match_ [%expr lhs, rhs] cases]]
    | { ptyp_desc = Ptyp_var name } -> evar ("poly_"^name)
    | { ptyp_desc = Ptyp_alias (typ, _) } -> expr_of_typ typ
    | { ptyp_loc } ->
      raise_errorf ~loc:ptyp_loc "Cannot derive eq for %s"
                   (Ppx_deriving.string_of_core_type typ)

let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  let comparator =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest -> expr_of_typ manifest
    | Ptype_variant constrs, _ ->
      let cases =
        (constrs |> List.map (fun { pcd_name = { txt = name }; pcd_args = typs } ->
          exprsn typs |>
          Ppx_deriving.(fold_exprs ~unit:[%expr true] (binop_reduce [%expr (&&)])) |>
          Exp.case (ptuple [pconstr name (pattn `lhs typs);
                            pconstr name (pattn `rhs typs)]))) @
        [Exp.case (pvar "_") [%expr false]]
      in
      [%expr fun lhs rhs -> [%e Exp.match_ [%expr lhs, rhs] cases]]
    | Ptype_record labels, _ ->
      let exprs =
        labels |> List.map (fun { pld_name = { txt = name }; pld_type } ->
          let field obj = Exp.field obj (mknoloc (Lident name)) in
          app (expr_of_typ pld_type) [field (evar "lhs"); field (evar "rhs")])
      in
      [%expr fun lhs rhs -> [%e exprs |> Ppx_deriving.(fold_exprs (binop_reduce [%expr (&&)]))]]
    | Ptype_abstract, None ->
      raise_errorf ~loc "Cannot derive eq for fully abstract type"
    | Ptype_open, _ ->
      raise_errorf ~loc "Cannot derive eq for open type"
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  [Vb.mk (pvar (Ppx_deriving.mangle_type_decl (`Prefix "equal") type_decl))
         (polymorphize comparator)]

let sig_of_type ~options ~path type_decl =
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let polymorphize = Ppx_deriving.poly_arrow_of_type_decl
          (fun var -> [%type: [%t var] -> [%t var] -> bool]) type_decl in
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "equal") type_decl))
              (polymorphize [%type: [%t typ] -> [%t typ] -> bool]))]

let () =
  Ppx_deriving.(register "eq" {
    core_type = Some expr_of_typ;
    structure = (fun ~options ~path type_decls ->
      [Str.value Recursive (List.concat (List.map (str_of_type ~options ~path) type_decls))]);
    signature = (fun ~options ~path type_decls ->
      List.concat (List.map (sig_of_type ~options ~path) type_decls));
  })
