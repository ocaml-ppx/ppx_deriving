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
    let expr_of_type ({ ptype_name = { txt = name }; ptype_loc } as type_) =
      let rec expr_of_typ lhs rhs typ =
        match Ppx_deriving.attr ~prefix "equal" typ.ptyp_attributes with
        | Some (_, PStr [{ pstr_desc = Pstr_eval (equal, _) }]) ->
          [%expr [%e equal] [%e lhs] [%e rhs]]
        | Some ({ loc }, _) ->
          raise_errorf ~loc "Invalid [@%s.equal] syntax" prefix
        | None ->
          match typ with
          | [%type: int] | [%type: int32] | [%type: Int32.t]
          | [%type: int64] | [%type: Int64.t] | [%type: nativeint] | [%type: Nativeint.t]
          | [%type: float] | [%type: bool] | [%type: char] | [%type: string] | [%type: bytes] ->
            [%expr (fun (a:[%t typ]) b -> a = b) [%e lhs] [%e rhs]]
          | { ptyp_desc = Ptyp_tuple typs } ->
            assert false
            (* begin match List.mapi (fun i typ -> expr_of_typ (evar (argn i)) typ) typs with
            | [] -> assert false
            | hd :: tl ->
              [%expr
                let [%p ptuple (List.mapi (fun i _ -> pvar (argn i)) typs)] = [%e expr] in
                Format.pp_print_string fmt "(";
                [%e List.fold_left (fun exp exp' ->
                      [%expr [%e exp]; Format.pp_print_string fmt ", "; [%e exp']])
                    hd tl];
                Format.pp_print_string fmt ")"]
            end *)
          | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
            app (Exp.ident (mknoloc (Ppx_deriving.mangle_lid ~prefix:"equal_" lid)))
                ([%expr fmt] ::
                 (List.map (fun typ ->
                    [%expr fun lhs rhs -> [%e expr_of_typ [%expr lhs] [%expr rhs] typ]])
                  args) @
                 [lhs; rhs])
          | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
            assert false
            (* let cases =
              fields |> List.map (fun field ->
                match field with
                | Rtag (label, _, true (*empty*), []) ->
                  Exp.case (Pat.variant label None)
                           [%expr Format.pp_print_string fmt [%e str ("`" ^ label)]]
                | Rtag (label, _, false, [typ]) ->
                  Exp.case (Pat.variant label (Some (pvar "x")))
                           [%expr Format.pp_print_string fmt [%e str ("`" ^ label ^ " ")];
                                  [%e expr_of_typ (evar "x") typ]]
                | Rinherit ({ ptyp_desc = Ptyp_constr (tname, []) } as typ) ->
                  Exp.case (Pat.alias (Pat.type_ tname) (mknoloc "x"))
                           (expr_of_typ (evar "x") typ)
                | _ ->
                  raise_errorf ~loc:ptyp_loc "Cannot derive Show for %s"
                               (Ppx_deriving.string_of_core_type typ))
            in
            Exp.match_ expr cases *)
          | { ptyp_desc = Ptyp_var name } ->
            [%expr [%e evar ("poly_"^name)] [%e lhs] [%e rhs]]
          | { ptyp_desc = Ptyp_alias (typ', _) } ->
            expr_of_typ lhs rhs typ'
          | { ptyp_loc } ->
            raise_errorf ~loc:ptyp_loc "Cannot derive Show for %s"
                         (Ppx_deriving.string_of_core_type typ)
      in
      let comparator =
        match type_.ptype_kind, type_.ptype_manifest with
        | Ptype_abstract, Some manifest ->
          [%expr fun lhs rhs -> [%e expr_of_typ [%expr lhs] [%expr rhs] manifest]]
        | Ptype_variant constrs, _ ->
          (* let cases =
            constrs |> List.map (fun { pcd_name = { txt = name }; pcd_args } ->
              let result =
                match List.mapi (fun i typ -> expr_of_typ (evar (argn i)) typ) pcd_args with
                | [] -> [%expr Format.pp_print_string fmt [%e str name]]
                | hd :: [] -> [%expr Format.pp_print_string fmt [%e str (name ^ " ")]; [%e hd]]
                | hd :: tl ->
                  [%expr
                    Format.pp_print_string fmt [%e str (name ^ " (")];
                    [%e List.fold_left (fun exp exp' ->
                          [%expr [%e exp]; Format.pp_print_string fmt ", "; [%e exp']])
                        hd tl];
                    Format.pp_print_string fmt ")"]
              in
              Exp.case (pconstr name (List.mapi (fun i _ -> pvar (argn i)) pcd_args)) result)
          in
          [%expr fun fmt x -> [%e Exp.match_ (evar "x") cases]] *)
          assert false
        | Ptype_record labels, _ ->
          (* let labels =
            labels |> List.map (fun { pld_name = { txt = name }; pld_type } ->
              [%expr
                Format.pp_print_string fmt [%e str (name ^ " = ")];
                [%e expr_of_typ (Exp.field (evar "x") (mknoloc (Lident name))) pld_type]])
          in
          let hd, tl = match labels with hd :: tl -> hd, tl | _ -> assert false in
          [%expr fun fmt x ->
            Format.pp_print_string fmt "{ ";
            [%e List.fold_left (fun exp exp' ->
                  [%expr [%e exp]; Format.pp_print_string fmt "; "; [%e exp']])
                hd tl];
            Format.pp_print_string fmt " }"] *)
          assert false
        | Ptype_abstract, None ->
          raise_errorf ~loc:ptype_loc "Cannot derive Eq for fully abstract type"
        | Ptype_open, _ ->
          raise_errorf ~loc:ptype_loc "Cannot derive Eq for open type"
      in
      let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_ in
      [Vb.mk (pvar ("equal_"^name)) (polymorphize comparator)]
    in
    let sig_of_type { ptype_name = { txt = name }; ptype_params } =
      let typ = Typ.constr (lid name) (List.map fst ptype_params) in
      [Sig.value (Val.mk (mknoloc ("equal_"^name)) [%type: [%t typ] -> [%t typ] -> bool])]
    in
    [Str.value Recursive (List.concat (List.map expr_of_type type_decls))],
    List.concat (List.map sig_of_type type_decls))
