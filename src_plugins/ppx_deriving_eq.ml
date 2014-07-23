open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let prefix = "eq"
let raise_errorf = Ppx_deriving.raise_errorf

let () =
  Ppx_deriving.register "Eq" (fun options type_decls ->
    let expr_of_type ({ ptype_name = { txt = name }; ptype_loc = loc } as type_) =
      let argn kind = Printf.sprintf (match kind with `lhs -> "lhs%d" | `rhs -> "rhs%d") in
      let pattn side typs = List.mapi (fun i _ -> pvar (argn side i)) typs in
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
          | { ptyp_desc = Ptyp_tuple typs } ->
            [%expr fun [%p ptuple (pattn `lhs typs)] [%p ptuple (pattn `rhs typs)] ->
              [%e exprsn typs |> Ppx_deriving.(fold_exprs (binop_reduce [%expr (&&)]))]]
          | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
            (* ppx_tools#10 *)
            let fn = Exp.ident (mknoloc (Ppx_deriving.mangle_lid ~prefix:"equal_" lid)) in
            (match args with [] -> fn | _ -> app fn (List.map expr_of_typ args))
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
                  raise_errorf ~loc:ptyp_loc "Cannot derive Show for %s"
                               (Ppx_deriving.string_of_core_type typ))) @
              [Exp.case (pvar "_") [%expr false]]
            in
            [%expr fun lhs rhs -> [%e Exp.match_ [%expr lhs, rhs] cases]]
          | { ptyp_desc = Ptyp_var name } -> evar ("poly_"^name)
          | { ptyp_desc = Ptyp_alias (typ, _) } -> expr_of_typ typ
          | { ptyp_loc } ->
            raise_errorf ~loc:ptyp_loc "Cannot derive Show for %s"
                         (Ppx_deriving.string_of_core_type typ)
      in
      let comparator =
        match type_.ptype_kind, type_.ptype_manifest with
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
          raise_errorf ~loc "Cannot derive Eq for fully abstract type"
        | Ptype_open, _ ->
          raise_errorf ~loc "Cannot derive Eq for open type"
      in
      let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_ in
      [Vb.mk (pvar ("equal_"^name)) (polymorphize comparator)]
    in
    let sig_of_type type_ =
      let typ = Ppx_deriving.typ_of_type_decl type_ in
      let polymorphize = Ppx_deriving.poly_arrow_of_type_decl
              (fun var -> [%type: [%t var] -> [%t var] -> bool]) type_ in
      [Sig.value (Val.mk (mknoloc ("equal_"^type_.ptype_name.txt))
                  (polymorphize [%type: [%t typ] -> [%t typ] -> bool]))]
    in
    [Str.value Recursive (List.concat (List.map expr_of_type type_decls))],
    List.concat (List.map sig_of_type type_decls))
