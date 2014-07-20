open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let prefix = "show"
let raise_errorf = Ppx_deriving.raise_errorf
let mangle_lid = Ppx_deriving.mangle_lid

let () =
  Ppx_deriving.register "Show" (fun options type_decls ->
    let argn i = Printf.sprintf "a%d" i in
    let rec expr_of_typ expr typ =
      match Ppx_deriving.attr ~prefix "format" typ.ptyp_attributes with
      | Some (_, PStr [{ pstr_desc = Pstr_eval (printer, _) }]) ->
        [%expr [%e printer] fmt [%e expr]]
      | Some ({ loc }, _) ->
        raise_errorf ~loc "Invalid [@%s.format] syntax" prefix
      | None ->
        let format x = [%expr Format.fprintf fmt [%e str x] [%e expr]] in
        match typ with
        | [%type: int]    -> format "%d"
        | [%type: int32]     | [%type: Int32.t] -> format "%ldl"
        | [%type: int64]     | [%type: Int64.t] -> format "%LdL"
        | [%type: nativeint] | [%type: Nativeint.t] -> format "%ndn"
        | [%type: float]  -> format "%F"
        | [%type: bool]   -> format "%B"
        | [%type: char]   -> format "%C"
        | [%type: string] -> format "%S"
        | [%type: bytes]  -> [%expr Format.fprintf fmt "%S" (Bytes.to_string [%e expr])]
        | { ptyp_desc = Ptyp_tuple typs } ->
          begin match List.mapi (fun i typ -> expr_of_typ (evar (argn i)) typ) typs with
          | [] -> assert false
          | hd :: tl ->
            [%expr
              let [%p ptuple (List.mapi (fun i _ -> pvar (argn i)) typs)] = [%e expr] in
              Format.pp_print_string fmt "(";
              [%e List.fold_left (fun exp exp' ->
                    [%expr [%e exp]; Format.pp_print_string fmt ", "; [%e exp']])
                  hd tl];
              Format.pp_print_string fmt ")"]
          end
        | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
          app (Exp.ident { txt = mangle_lid ~prefix:"pp_" lid; loc = !default_loc })
              ([%expr fmt] ::
               (List.map (fun typ -> [%expr fun x -> [%e expr_of_typ [%expr x] typ]]) args) @
               [expr])
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
                                [%e expr_of_typ (evar "x") typ]]
              | Rinherit ({ ptyp_desc = Ptyp_constr (tname, []) } as typ) ->
                Exp.case (Pat.alias (Pat.type_ tname) (mknoloc "x"))
                         (expr_of_typ (evar "x") typ)
              | _ ->
                raise_errorf ~loc:ptyp_loc "Cannot derive Show for %s"
                             (Ppx_deriving.string_of_core_type typ))
          in
          Exp.match_ expr cases
        | { ptyp_desc = Ptyp_alias (typ', _) } -> expr_of_typ expr typ'
        | { ptyp_loc } ->
          raise_errorf ~loc:ptyp_loc "Cannot derive Show for %s"
                       (Ppx_deriving.string_of_core_type typ)
    in
    let expr_of_type { ptype_name = { txt = name }; ptype_kind; ptype_manifest; ptype_loc } =
      let prettyprinter =
        match ptype_kind, ptype_manifest with
        | Ptype_abstract, Some manifest ->
          [%expr fun fmt x -> [%e expr_of_typ (evar "x") manifest]]
        | Ptype_variant constrs, _ ->
          let cases =
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
          [%expr fun fmt x -> [%e Exp.match_ (evar "x") cases]]
        | Ptype_record labels, _ ->
          let labels =
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
            Format.pp_print_string fmt " }"]
        | Ptype_abstract, None ->
          raise_errorf ~loc:ptype_loc "Cannot derive Show for fully abstract type"
        | Ptype_open, _ ->
          raise_errorf ~loc:ptype_loc "Cannot derive Show for open type"
      in
      let stringprinter = [%expr fun x -> Format.asprintf "%a" [%e evar ("pp_"^name)] x] in
      [Vb.mk (pvar ("pp_"^name))   prettyprinter;
       Vb.mk (pvar ("show_"^name)) stringprinter;]
    in
    let sig_of_type { ptype_name = { txt = name }; ptype_params } =
      let typ = Typ.constr (lid name) (List.map fst ptype_params) in
      [Sig.value (Val.mk (mknoloc ("pp_"^name))   [%type: Format.formatter -> [%t typ] -> unit]);
       Sig.value (Val.mk (mknoloc ("show_"^name)) [%type: [%t typ] -> string])]
    in
    [Str.value Recursive (List.concat (List.map expr_of_type type_decls))],
    List.concat (List.map sig_of_type type_decls))
