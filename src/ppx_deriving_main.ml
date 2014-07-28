open Longident
open Location
open Asttypes
open Parsetree
open Ast_mapper
open Ast_helper
open Ast_convenience

let raise_errorf = Ppx_deriving.raise_errorf

let () = Findlib.init ()

let dynlink ?(loc=Location.none) filename =
  try
    Dynlink.loadfile filename
  with Dynlink.Error error ->
    raise_errorf ~loc "Cannot load %s: %s" filename (Dynlink.error_message error)

let load_deriver loc name =
  let libname = String.lowercase name in
  let pkgname, pkglist =
    try  let pkgname = "ppx_deriving."^libname in
         pkgname, Findlib.package_ancestors [] pkgname
    with Fl_package_base.No_such_package _ ->
      try  let pkgname = "ppx_deriving_"^libname in
           pkgname, Findlib.package_ancestors [] pkgname
      with Fl_package_base.No_such_package _ ->
        raise_errorf ~loc "Cannot locate findlib package ppx_deriving.%s or ppx_deriving_%s"
                          libname libname
  in
  let pkglist = List.filter ((<>) "ppx_deriving.api") pkglist in
  let pkglist = pkgname :: Findlib.package_deep_ancestors [] pkglist in
  pkglist |> List.iter (fun pkg ->
    let kind = if Dynlink.is_native then "native" else "byte" in
    Findlib.package_property [kind; "plugin"] pkg "archive" |>
    Findlib.resolve_path ~base:(Findlib.package_directory pkg) |>
    dynlink ~loc);
  begin match Ppx_deriving.lookup name with
  | Some deriver -> deriver
  | None -> raise_errorf ~loc "Expected packages %s to define a deriver %s"
                              (String.concat ", " pkglist) name
  end

let derive_type_decl path typ_decls pstr_loc item fn =
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
          name, options |> List.map (fun ({ txt }, expr) ->
            String.concat "." (Longident.flatten txt), expr)
        | { pexp_loc } ->
          raise_errorf ~loc:pexp_loc "Unrecognized [@@deriving] option syntax"
      in
      let name, loc = String.concat "_" (Longident.flatten name.txt), name.loc in
      let deriver =
        match Ppx_deriving.lookup name with
        | Some deriver -> deriver
        | None -> load_deriver loc name
      in
      items @ ((fn deriver) ~options ~path:(!path) typ_decls))
    [item] deriver_exprs

let module_from_input_name () =
  match !Location.input_name with
  | "//toplevel//" -> []
  | filename -> [String.capitalize (Filename.(basename (chop_suffix filename ".ml")))]

let mapper argv =
  List.iter (fun file -> dynlink (Dynlink.adapt_filename file)) argv;
  let module_nesting = ref [] in
  let with_module name f =
    let old_nesting = !module_nesting in
    module_nesting := !module_nesting @ [name];
    let result = f () in
    module_nesting := old_nesting;
    result
  in
  let expression mapper expr =
    match expr with
    | { pexp_desc = Pexp_extension ({ txt = name; loc }, payload) }
        when String.(length name >= 7 && sub name 0 7 = "derive.") ->
      let name = String.sub name 7 ((String.length name) - 7) in
      let deriver =
        match Ppx_deriving.lookup name with
        | Some deriver -> deriver
        | None -> load_deriver loc name
      in
      begin match payload with
      | PTyp typ -> deriver.Ppx_deriving.core_type typ
      | _ -> raise_errorf ~loc "Unrecognized [%%derive.*] annotation syntax"
      end
    | _ -> default_mapper.expr mapper expr
  in
  let rec structure mapper items =
    match items with
    | { pstr_desc = Pstr_type typ_decls; pstr_loc } as item :: rest when
        List.exists (fun ty -> has_attr "deriving" ty.ptype_attributes) typ_decls ->
      Ast_helper.with_default_loc pstr_loc (fun () ->
        derive_type_decl module_nesting typ_decls pstr_loc item
          (fun deriver -> deriver.Ppx_deriving.structure) @ structure mapper rest)
    | { pstr_desc = Pstr_module ({ pmb_name = { txt = name } } as mb) } as item :: rest ->
      { item with pstr_desc = Pstr_module (
          with_module name (fun () -> mapper.module_binding mapper mb)) }
        :: structure mapper rest
    | { pstr_desc = Pstr_recmodule mbs } as item :: rest ->
      { item with pstr_desc = Pstr_recmodule (
          mbs |> List.map (fun ({ pmb_name = { txt = name } } as mb) ->
            with_module name (fun () -> mapper.module_binding mapper mb))) }
        :: structure mapper rest
    | { pstr_loc } as item :: rest ->
      mapper.structure_item mapper item :: structure mapper rest
    | [] -> []
  in
  let rec signature mapper items =
    match items with
    | { psig_desc = Psig_type typ_decls; psig_loc } as item :: rest when
        List.exists (fun ty -> has_attr "deriving" ty.ptype_attributes) typ_decls ->
      Ast_helper.with_default_loc psig_loc (fun () ->
        derive_type_decl module_nesting typ_decls psig_loc item
          (fun deriver -> deriver.Ppx_deriving.signature) @ signature mapper rest)
    | { psig_desc = Psig_module ({ pmd_name = { txt = name } } as md) } as item :: rest ->
      { item with psig_desc = Psig_module (
          with_module name (fun () -> mapper.module_declaration mapper md)) }
        :: signature mapper rest
    | { psig_desc = Psig_recmodule mds } as item :: rest ->
      { item with psig_desc = Psig_recmodule (
          mds |> List.map (fun ({ pmd_name = { txt = name } } as md) ->
            with_module name (fun () -> mapper.module_declaration mapper md))) }
        :: signature mapper rest
    | { psig_loc } as item :: rest ->
      mapper.signature_item mapper item :: signature mapper rest
    | [] -> []
  in
  { default_mapper with
    expr = expression;
    structure = (fun mapper items ->
      module_nesting := module_from_input_name ();
      structure { mapper with structure; signature } items);
    signature = (fun mapper items ->
      module_nesting := module_from_input_name ();
      signature { mapper with structure; signature } items)
  }

let () =
  Ast_mapper.register "ppx_deriving" mapper

