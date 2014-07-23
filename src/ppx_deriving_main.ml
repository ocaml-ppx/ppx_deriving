open Longident
open Location
open Asttypes
open Parsetree
open Ast_mapper
open Ast_helper
open Ast_convenience

let raise_errorf = Ppx_deriving.raise_errorf

let string_of_lident lid =
  String.concat "." (Longident.flatten lid)

let load_deriver loc name =
  let libname = String.lowercase name in
  let try_expand filename =
    let filename = Dynlink.adapt_filename filename in
    try  Some (filename, Findlib.resolve_path filename)
    with Fl_package_base.No_such_package _ -> None
  in
  let filename, filepath =
    match try_expand (Printf.sprintf "@ppx_deriving_%s/ppx_deriving_%s.cmo" libname libname) with
    | Some x -> x
    | None ->
      match try_expand (Printf.sprintf "@ppx_deriving.%s/ppx_deriving_%s.cmo" libname libname) with
      | Some x -> x
      | None ->
        raise_errorf ~loc "Cannot locate findlib package ppx_deriving.%s or ppx_deriving_%s"
                          libname libname
  in
  try
    Dynlink.loadfile filepath;
    begin match Ppx_deriving.lookup name with
    | Some deriver -> deriver
    | None -> raise_errorf ~loc "Expected %s to define a deriver %s" filename name
    end
  with Dynlink.Error error ->
    raise_errorf ~loc "Cannot load %s: %s" filename (Dynlink.error_message error)

let derive typ_decls pstr_loc item fn =
  let attributes = List.concat (List.map (fun { ptype_attributes = attrs } -> attrs) typ_decls) in
  let deriving = find_attr "deriving" attributes in
  let deriver_exprs, loc =
    match deriving with
    | Some (PStr [{ pstr_desc = Pstr_eval (
                    { pexp_desc = Pexp_tuple exprs }, []); pstr_loc }]) ->
      exprs, pstr_loc
    | Some (PStr [{ pstr_desc = Pstr_eval (
                    { pexp_desc = Pexp_construct _ } as expr, []); pstr_loc }]) ->
      [expr], pstr_loc
    | _ -> raise_errorf ~loc:pstr_loc "Unrecognized [@@deriving] annotation syntax"
  in
  List.fold_left (fun items deriver_expr ->
      let name, options =
        match deriver_expr with
        | { pexp_desc = Pexp_construct (name, None) } ->
          name, []
        | { pexp_desc = Pexp_construct (name, Some
            { pexp_desc = Pexp_record (options, None) }) } ->
          name, List.map (fun ({ txt }, expr) -> string_of_lident txt, expr) options
        | { pexp_loc } ->
          raise_errorf ~loc:pexp_loc "Unrecognized [@@deriving] option syntax"
      in
      let name, loc = String.concat "_" (Longident.flatten name.txt), name.loc in
      let deriver =
        match Ppx_deriving.lookup name with
        | Some deriver -> deriver
        | None -> load_deriver loc name
      in
      items @ (fn (deriver options typ_decls)))
    [item] deriver_exprs

let mapper argv =
  List.iter (fun f -> Dynlink.(loadfile (adapt_filename f))) argv;
  { default_mapper with
    structure = (fun mapper items ->
      let rec map_types items =
        match items with
        | { pstr_desc = Pstr_type typ_decls; pstr_loc } as item :: rest when
            List.exists (fun ty -> has_attr "deriving" ty.ptype_attributes) typ_decls ->
          Ast_helper.with_default_loc pstr_loc (fun () ->
            derive typ_decls pstr_loc item fst @ map_types rest)
        | { pstr_loc } as item :: rest ->
          mapper.structure_item mapper item :: map_types rest
        | [] -> []
      in
      map_types items);
    signature = (fun mapper items ->
      let rec map_types items =
        match items with
        | { psig_desc = Psig_type typ_decls; psig_loc } as item :: rest when
            List.exists (fun ty -> has_attr "deriving" ty.ptype_attributes) typ_decls ->
          Ast_helper.with_default_loc psig_loc (fun () ->
            derive typ_decls psig_loc item snd @ map_types rest)
        | { psig_loc } as item :: rest ->
          mapper.signature_item mapper item :: map_types rest
        | [] -> []
      in
      map_types items)
  }

let () =
  Ast_mapper.register "ppx_deriving" mapper

