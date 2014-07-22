open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let prefix = "show"
let raise_errorf = Ppx_deriving.raise_errorf

let () =
  Ppx_deriving.register "Show" (fun options type_decls ->
    let argn i = Printf.sprintf "a%d" i in
    let rec expr_of_typ typ =
      match Ppx_deriving.attr ~prefix "printer" typ.ptyp_attributes with
      | Some (_, PStr [{ pstr_desc = Pstr_eval (printer, _) }]) -> [%expr [%e printer] fmt]
      | Some ({ loc }, _) -> raise_errorf ~loc "Invalid [@%s.printer] syntax" prefix
      | None ->
        let format x = [%expr Format.fprintf fmt [%e str x]] in
        match typ with
        | [%type: int]    -> format "%d"
        | [%type: int32]     | [%type: Int32.t] -> format "%ldl"
        | [%type: int64]     | [%type: Int64.t] -> format "%LdL"
        | [%type: nativeint] | [%type: Nativeint.t] -> format "%ndn"
        | [%type: float]  -> format "%F"
        | [%type: bool]   -> format "%B"
        | [%type: char]   -> format "%C"
        | [%type: string] -> format "%S"
        | [%type: bytes]  -> [%expr fun x -> Format.fprintf fmt "%S" (Bytes.to_string x)]
        | { ptyp_desc = Ptyp_tuple typs } ->
          let args = List.mapi (fun i typ -> app (expr_of_typ typ) [evar (argn i)]) typs in
          [%expr
            fun [%p ptuple (List.mapi (fun i _ -> pvar (argn i)) typs)] ->
            Format.pp_print_string fmt "(";
            [%e args |> Ppx_deriving.(fold_exprs
                    (seq_reduce [%expr Format.pp_print_string fmt ", "]))];
            Format.pp_print_string fmt ")"]
        | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
          app (Exp.ident (mknoloc (Ppx_deriving.mangle_lid ~prefix:"pp_" lid)))
              ([%expr fmt] :: (List.map expr_of_typ args))
        | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
          let cases =
            fields |> List.map (fun field ->
              match field with
              | Rtag (label, _, true (*empty*), []) ->
                Exp.case (Pat.variant label None)
                         [%expr Format.pp_print_string fmt [%e str ("`" ^ label)]]
              | Rtag (label, _, false, [typ]) ->
                Exp.case (Pat.variant label (Some (pvar "x")))
                         [%expr Format.pp_print_string fmt [%e str ("`" ^ label ^ " ")];
                                [%e expr_of_typ typ] x]
              | Rinherit ({ ptyp_desc = Ptyp_constr (tname, []) } as typ) ->
                Exp.case (Pat.alias (Pat.type_ tname) (mknoloc "x"))
                         [%expr [%e expr_of_typ typ] x]
              | _ ->
                raise_errorf ~loc:ptyp_loc "Cannot derive Show for %s"
                             (Ppx_deriving.string_of_core_type typ))
          in
          Exp.function_ cases
        | { ptyp_desc = Ptyp_var name } -> [%expr [%e evar ("poly_"^name)] fmt]
        | { ptyp_desc = Ptyp_alias (typ, _) } -> expr_of_typ typ
        | { ptyp_loc } ->
          raise_errorf ~loc:ptyp_loc "Cannot derive Show for %s"
                       (Ppx_deriving.string_of_core_type typ)
    in
    let expr_of_type ({ ptype_name = { txt = name }; ptype_loc = loc } as type_) =
      let prettyprinter =
        match type_.ptype_kind, type_.ptype_manifest with
        | Ptype_abstract, Some manifest ->
          [%expr fun fmt -> [%e expr_of_typ manifest]]
        | Ptype_variant constrs, _ ->
          let cases =
            constrs |> List.map (fun { pcd_name = { txt = name }; pcd_args } ->
              let args = List.mapi (fun i typ -> app (expr_of_typ typ) [evar (argn i)]) pcd_args in
              let result =
                match args with
                | []   -> [%expr Format.pp_print_string fmt [%e str name]]
                | [a]  -> [%expr Format.pp_print_string fmt [%e str (name ^ " ")]; [%e a]]
                | args -> [%expr Format.pp_print_string fmt [%e str (name ^ " (")];
                  [%e args |> Ppx_deriving.(fold_exprs
                        (seq_reduce [%expr Format.pp_print_string fmt ", "]))];
                  Format.pp_print_string fmt ")"]
              in
              Exp.case (pconstr name (List.mapi (fun i _ -> pvar (argn i)) pcd_args)) result)
          in
          [%expr fun fmt -> [%e Exp.function_ cases]]
        | Ptype_record labels, _ ->
          let fields =
            labels |> List.map (fun { pld_name = { txt = name }; pld_type } ->
              [%expr Format.pp_print_string fmt [%e str (name ^ " = ")];
                [%e expr_of_typ pld_type] [%e Exp.field (evar "x") (mknoloc (Lident name))]])
          in
          [%expr fun fmt x ->
            Format.pp_print_string fmt "{ ";
            [%e fields |> Ppx_deriving.(fold_exprs
                  (seq_reduce [%expr Format.pp_print_string fmt "; "]))];
            Format.pp_print_string fmt " }"]
        | Ptype_abstract, None -> raise_errorf ~loc "Cannot derive Show for fully abstract type"
        | Ptype_open, _        -> raise_errorf ~loc "Cannot derive Show for open type"
      in
      let pp_poly_apply = Ppx_deriving.poly_apply_of_type_decl type_ (evar ("pp_"^name)) in
      let stringprinter = [%expr fun x -> Format.asprintf "%a" [%e pp_poly_apply] x] in
      let polymorphize  = Ppx_deriving.poly_fun_of_type_decl type_ in
      [Vb.mk (pvar ("pp_"^name))   (polymorphize prettyprinter);
       Vb.mk (pvar ("show_"^name)) (polymorphize stringprinter);]
    in
    let sig_of_type ({ ptype_name = { txt = name }; ptype_params } as type_) =
      let typ = Typ.constr (lid name) (List.map fst ptype_params) in
      let polymorphize = Ppx_deriving.poly_arrow_of_type_decl
            (fun var -> [%type: Format.formatter -> [%t var] -> unit]) type_ in
      [Sig.value (Val.mk (mknoloc ("pp_"^name))
                 (polymorphize [%type: Format.formatter -> [%t typ] -> unit]));
       Sig.value (Val.mk (mknoloc ("show_"^name))
                 (polymorphize [%type: [%t typ] -> string]))]
    in
    [Str.value Recursive (List.concat (List.map expr_of_type type_decls))],
    List.concat (List.map sig_of_type type_decls))
