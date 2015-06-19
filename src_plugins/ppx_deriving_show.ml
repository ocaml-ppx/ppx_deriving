open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

module StringSet = Ppx_deriving.StringSet

let deriver = "show"
let raise_errorf = Ppx_deriving.raise_errorf

type show_options =
  {
    allow_std_type_shadowing: bool;
  }

let default_show_options =
  {
    allow_std_type_shadowing = false;
  }

let parse_options options =
  let option_parser acc (name, expr) =
    match name with
    | "allow_std_type_shadowing" -> { allow_std_type_shadowing = true }
    | _ ->
      raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name in
  List.fold_left option_parser default_show_options options

let attr_printer attrs =
  Ppx_deriving.(attrs |> attr ~deriver "printer" |> Arg.(get_attr ~deriver expr))

let attr_polyprinter attrs =
  Ppx_deriving.(attrs |> attr ~deriver "polyprinter" |> Arg.(get_attr ~deriver expr))

let attr_opaque attrs =
  Ppx_deriving.(attrs |> attr ~deriver "opaque" |> Arg.get_flag ~deriver)

let argn = Printf.sprintf "a%d"

let pp_type_of_decl ~options ~path type_decl =
  ignore (parse_options options);
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: Format.formatter -> [%t var] -> unit])
    type_decl
    [%type: Format.formatter -> [%t typ] -> unit]

let show_type_of_decl ~options ~path type_decl =
  ignore (parse_options options);
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: Format.formatter -> [%t var] -> unit])
    type_decl
    [%type: [%t typ] -> String.t] (* using String.t in case of std type hiding. *)

let sig_of_type ~options ~path type_decl =
  ignore (parse_options options);
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "pp") type_decl))
              (pp_type_of_decl ~options ~path type_decl));
   Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "show") type_decl))
              (show_type_of_decl ~options ~path type_decl))]

let rec expr_of_typ group_def typ =
  let generate_printer_call lid args =
    let args_pp = List.map (fun typ -> [%expr fun fmt -> [%e expr_of_typ group_def typ]]) args in
    match attr_polyprinter typ.ptyp_attributes with
    | Some printer ->
      app [%expr (let fprintf = Format.fprintf in [%e printer]) [@ocaml.warning "-26"]]
          (args_pp @ [[%expr fmt]])
    | None ->
      app (Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "pp") lid)))
          (args_pp @ [[%expr fmt]]) in
  match attr_printer typ.ptyp_attributes with
  | Some printer ->
    [%expr (let fprintf = Format.fprintf in [%e printer]) fmt [@ocaml.warning "-26"]]
  | None ->
  if attr_opaque typ.ptyp_attributes then
    [%expr fun _ -> Format.pp_print_string fmt "<opaque>"]
  else
    let format x = [%expr Format.fprintf fmt [%e str x]] in
    let seq start finish fold typ =
      [%expr fun x ->
        Format.fprintf fmt [%e str start];
        ignore ([%e fold] (fun sep x ->
          if sep then Format.fprintf fmt ";@ ";
          [%e expr_of_typ group_def typ] x; true) false x);
        Format.fprintf fmt [%e str finish];]
    in
    match typ with
    | { ptyp_desc = Ptyp_constr ({ txt = (Lident id as lid) }, args) } when
            StringSet.mem id group_def -> generate_printer_call lid args
    | [%type: _]      -> [%expr fun _ -> Format.pp_print_string fmt "_"]
    | [%type: unit]   -> [%expr fun () -> Format.pp_print_string fmt "()"]
    | [%type: int]    -> format "%d"
    | [%type: int32]     | [%type: Int32.t] -> format "%ldl"
    | [%type: int64]     | [%type: Int64.t] -> format "%LdL"
    | [%type: nativeint] | [%type: Nativeint.t] -> format "%ndn"
    | [%type: float]  -> format "%F"
    | [%type: bool]   -> format "%B"
    | [%type: char]   -> format "%C"
    | [%type: string] | [%type: String.t] -> format "%S"
    | [%type: bytes]  -> [%expr fun x -> Format.fprintf fmt "%S" (Bytes.to_string x)]
    | [%type: [%t? typ] ref]   ->
      [%expr fun x ->
        Format.pp_print_string fmt "ref (";
        [%e expr_of_typ group_def typ] !x;
        Format.pp_print_string fmt ")"]
    | [%type: [%t? typ] list]  -> seq "[@[<hov>"   "@]]" [%expr List.fold_left]  typ
    | [%type: [%t? typ] array] -> seq "[|@[<hov>" "@]|]" [%expr Array.fold_left] typ
    | [%type: [%t? typ] option] ->
      [%expr
        function
        | None -> Format.pp_print_string fmt "None"
        | Some x ->
          Format.pp_print_string fmt "(Some ";
          [%e expr_of_typ group_def typ] x;
          Format.pp_print_string fmt ")"]
    | { ptyp_desc = Ptyp_arrow _ } ->
      [%expr fun _ -> Format.pp_print_string fmt "<fun>"]
    | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
        generate_printer_call lid args
    | { ptyp_desc = Ptyp_tuple typs } ->
      let args = List.mapi (fun i typ -> app (expr_of_typ group_def typ) [evar (argn i)]) typs in
      [%expr
        fun [%p ptuple (List.mapi (fun i _ -> pvar (argn i)) typs)] ->
        Format.fprintf fmt "(@[<hov>";
        [%e args |> Ppx_deriving.(fold_exprs
                (seq_reduce ~sep:[%expr Format.fprintf fmt ",@ "]))];
        Format.fprintf fmt "@])"]
    | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
      let cases =
        fields |> List.map (fun field ->
          match field with
          | Rtag (label, _, true (*empty*), []) ->
            Exp.case (Pat.variant label None)
                     [%expr Format.pp_print_string fmt [%e str ("`" ^ label)]]
          | Rtag (label, _, false, [typ]) ->
            Exp.case (Pat.variant label (Some [%pat? x]))
                     [%expr Format.fprintf fmt [%e str ("`" ^ label ^ " (@[<hov>")];
                            [%e expr_of_typ group_def typ] x;
                            Format.fprintf fmt "@])"]
          | Rinherit ({ ptyp_desc = Ptyp_constr (tname, _) } as typ) ->
            Exp.case [%pat? [%p Pat.type_ tname] as x]
                     [%expr [%e expr_of_typ group_def typ] x]
          | _ ->
            raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                         deriver (Ppx_deriving.string_of_core_type typ))
      in
      Exp.function_ cases
    | { ptyp_desc = Ptyp_var name } -> [%expr [%e evar ("poly_"^name)] fmt]
    | { ptyp_desc = Ptyp_alias (typ, _) } -> expr_of_typ group_def typ
    | { ptyp_loc } ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                   deriver (Ppx_deriving.string_of_core_type typ)

let str_of_type ~options ~path group_def ({ ptype_loc = loc } as type_decl) =
  ignore (parse_options options);
  let path = Ppx_deriving.path_of_type_decl ~path type_decl in
  let prettyprinter =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest ->
      [%expr fun fmt -> [%e expr_of_typ group_def manifest]]
    | Ptype_variant constrs, _ ->
      let cases =
        constrs |> List.map (fun { pcd_name = { txt = name' }; pcd_args } ->
          let constr_name = Ppx_deriving.expand_path ~path name' in
          let args = List.mapi (fun i typ -> app (expr_of_typ group_def typ) [evar (argn i)]) pcd_args in
          let result =
            match args with
            | []   -> [%expr Format.pp_print_string fmt [%e str constr_name]]
            | [arg] ->
              [%expr
                Format.fprintf fmt [%e str ("(@[<hov2>" ^  constr_name ^ "@ ")];
                [%e arg];
                Format.fprintf fmt "@])"]
            | args ->
              [%expr Format.fprintf fmt [%e str ("@[<hov2>" ^  constr_name ^ " (@,")];
              [%e args |> Ppx_deriving.(fold_exprs
                    (seq_reduce ~sep:[%expr Format.fprintf fmt ",@ "]))];
              Format.fprintf fmt "@])"]
          in
          Exp.case (pconstr name' (List.mapi (fun i _ -> pvar (argn i)) pcd_args)) result)
      in
      [%expr fun fmt -> [%e Exp.function_ cases]]
    | Ptype_record labels, _ ->
      let fields =
        labels |> List.mapi (fun i { pld_name = { txt = name }; pld_type; pld_attributes } ->
          let field_name = if i = 0 then Ppx_deriving.expand_path ~path name else name in
          let pld_type = {pld_type with ptyp_attributes=pld_attributes@pld_type.ptyp_attributes} in
          [%expr Format.pp_print_string fmt [%e str (field_name ^ " = ")];
            [%e expr_of_typ group_def pld_type] [%e Exp.field (evar "x") (mknoloc (Lident name))]])
      in
      [%expr fun fmt x ->
        Format.fprintf fmt "{ @[<hov>";
        [%e fields |> Ppx_deriving.(fold_exprs
              (seq_reduce ~sep:[%expr Format.fprintf fmt ";@ "]))];
        Format.fprintf fmt "@] }"]
    | Ptype_abstract, None ->
      raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    | Ptype_open, _        ->
      raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let pp_poly_apply = Ppx_deriving.poly_apply_of_type_decl type_decl (evar
                        (Ppx_deriving.mangle_type_decl (`Prefix "pp") type_decl)) in
  let stringprinter = [%expr fun x -> Format.asprintf "%a" [%e pp_poly_apply] x] in
  let polymorphize  = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let pp_type =
    Ppx_deriving.strong_type_of_type @@ pp_type_of_decl ~options ~path type_decl in
  let show_type =
    Ppx_deriving.strong_type_of_type @@
      show_type_of_decl ~options ~path type_decl in
  let pp_var =
    pvar (Ppx_deriving.mangle_type_decl (`Prefix "pp") type_decl) in
  let show_var =
    pvar (Ppx_deriving.mangle_type_decl (`Prefix "show") type_decl) in
  [Vb.mk (Pat.constraint_ pp_var pp_type) (polymorphize prettyprinter);
   Vb.mk (Pat.constraint_ show_var show_type) (polymorphize stringprinter);]

let type_decl_str ~options ~path type_decls =
  let opts = parse_options options in
  let typename_set =
    Ppx_deriving.extract_typename_of_type_group 
      deriver
      ~allow_shadowing:opts.allow_std_type_shadowing
      type_decls in
  let code =
    List.map (str_of_type ~options ~path typename_set) type_decls in
  [Str.value Recursive (List.concat code)]

let () =
  Ppx_deriving.(register (create deriver
    ~core_type: (fun typ ->
      [%expr fun x -> Format.asprintf "%a" (fun fmt -> [%e expr_of_typ StringSet.empty typ]) x])
    ~type_decl_str: type_decl_str
    ~type_decl_sig: (fun ~options ~path type_decls ->
      List.concat (List.map (sig_of_type ~options ~path) type_decls))
    ()
  ))
