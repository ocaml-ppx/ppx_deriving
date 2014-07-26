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

let dynlink ?(loc=Location.none) filename filepath =
  try
    Dynlink.loadfile filepath
  with Dynlink.Error error ->
    raise_errorf ~loc "Cannot load %s: %s" filename (Dynlink.error_message error)

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
  dynlink ~loc filename filepath;
  begin match Ppx_deriving.lookup name with
  | Some deriver -> deriver
  | None -> raise_errorf ~loc "Expected %s to define a deriver %s" filename name
  end

let derive path typ_decls pstr_loc item fn =
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
      items @ (fn (deriver ~options ~path:(!path) typ_decls)))
    [item] deriver_exprs

let mapper argv =
  List.iter (fun file -> let file = Dynlink.adapt_filename file in dynlink file file) argv;
  let seen_toplevel = ref false in
  let module_nesting = ref [] in
  let set_module_nesting () =
    if not !seen_toplevel then begin
      seen_toplevel := true;
      module_nesting :=
        match !Location.input_name with
        | "//toplevel//" -> []
        | filename -> [String.capitalize (Filename.(basename (chop_suffix filename ".ml")))]
    end
  and with_module name f =
    let old_nesting = !module_nesting in
    module_nesting := !module_nesting @ [name];
    let result = f () in
    module_nesting := old_nesting;
    result
  in
  { default_mapper with
    structure = (fun mapper items ->
      set_module_nesting ();
      let rec map_types items =
        match items with
        | { pstr_desc = Pstr_type typ_decls; pstr_loc } as item :: rest when
            List.exists (fun ty -> has_attr "deriving" ty.ptype_attributes) typ_decls ->
          Ast_helper.with_default_loc pstr_loc (fun () ->
            derive module_nesting typ_decls pstr_loc item fst @ map_types rest)
        | { pstr_desc = Pstr_module ({ pmb_name = { txt = name } } as mb) } as item :: rest ->
          { item with pstr_desc = Pstr_module (
              with_module name (fun () -> mapper.module_binding mapper mb)) }
            :: map_types rest
        | { pstr_desc = Pstr_recmodule mbs } as item :: rest ->
          { item with pstr_desc = Pstr_recmodule (
              mbs |> List.map (fun ({ pmb_name = { txt = name } } as mb) ->
                with_module name (fun () -> mapper.module_binding mapper mb))) }
            :: map_types rest
        | { pstr_loc } as item :: rest ->
          mapper.structure_item mapper item :: map_types rest
        | [] -> []
      in
      map_types items);
    signature = (fun mapper items ->
      set_module_nesting ();
      let rec map_types items =
        match items with
        | { psig_desc = Psig_type typ_decls; psig_loc } as item :: rest when
            List.exists (fun ty -> has_attr "deriving" ty.ptype_attributes) typ_decls ->
          Ast_helper.with_default_loc psig_loc (fun () ->
            derive module_nesting typ_decls psig_loc item snd @ map_types rest)
        | { psig_desc = Psig_module ({ pmd_name = { txt = name } } as md) } as item :: rest ->
          { item with psig_desc = Psig_module (
              with_module name (fun () -> mapper.module_declaration mapper md)) }
            :: map_types rest
        | { psig_desc = Psig_recmodule mds } as item :: rest ->
          { item with psig_desc = Psig_recmodule (
              mds |> List.map (fun ({ pmd_name = { txt = name } } as md) ->
                with_module name (fun () -> mapper.module_declaration mapper md))) }
            :: map_types rest
        | { psig_loc } as item :: rest ->
          mapper.signature_item mapper item :: map_types rest
        | [] -> []
      in
      map_types items)
  }

let () =
  Ast_mapper.register "ppx_deriving" mapper

