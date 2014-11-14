open Longident
open Location
open Asttypes
open Parsetree
open Ast_mapper
open Ast_helper
open Ast_convenience

let raise_errorf = Ppx_deriving.raise_errorf

let dynlink ?(loc=Location.none) filename =
  let filename = Dynlink.adapt_filename filename in
  try
    Dynlink.loadfile filename
  with Dynlink.Error error ->
    raise_errorf ~loc "Cannot load %s: %s" filename (Dynlink.error_message error)

let get_plugins () =
  match Ast_mapper.get_cookie "ppx_deriving" with
  | Some { pexp_desc = Pexp_tuple exprs } ->
    exprs |> List.map (fun expr ->
      match expr with
      | { pexp_desc = Pexp_constant (Const_string (file, None)) } -> file
      | _ -> assert false)
  | Some _ -> assert false
  | None -> []

let add_plugins plugins =
  let loaded  = get_plugins () in
  let plugins = List.filter (fun file -> not (List.mem file loaded)) plugins in
  List.iter dynlink plugins;
  let loaded  = loaded @ plugins in
  Ast_mapper.set_cookie "ppx_deriving"
    (Exp.tuple (List.map (fun file -> Exp.constant (Const_string (file, None))) loaded))

let derive_type_decl path typ_decls pstr_loc item fn =
  let attributes = List.concat (List.map (fun { ptype_attributes = attrs } -> attrs) typ_decls) in
  let deriving = find_attr "deriving" attributes in
  let deriver_exprs, loc =
    match deriving with
    | Some (PStr [{ pstr_desc = Pstr_eval (
                    { pexp_desc = Pexp_tuple exprs }, []); pstr_loc }]) ->
      exprs, pstr_loc
    | Some (PStr [{ pstr_desc = Pstr_eval (
                    { pexp_desc = (Pexp_ident _ | Pexp_apply _) } as expr, []); pstr_loc }]) ->
      [expr], pstr_loc
    | _ -> raise_errorf ~loc:pstr_loc "Unrecognized [@@deriving] annotation syntax"
  in
  List.fold_left (fun items deriver_expr ->
      let name, options =
        match deriver_expr with
        | { pexp_desc = Pexp_ident name } ->
          name, []
        | { pexp_desc = Pexp_apply ({ pexp_desc = Pexp_ident name }, ["",
            { pexp_desc = Pexp_record (options, None) }]) } ->
          name, options |> List.map (fun ({ txt }, expr) ->
            String.concat "." (Longident.flatten txt), expr)
        | { pexp_loc } ->
          raise_errorf ~loc:pexp_loc "Unrecognized [@@deriving] option syntax"
      in
      let name, loc = String.concat "_" (Longident.flatten name.txt), name.loc in
      let is_optional, options =
        match List.assoc "optional" options with
        | exception Not_found -> false, options
        | expr ->
          Ppx_deriving.Arg.(get_expr ~deriver:name bool) expr,
          List.remove_assoc "optional" options
      in
      match Ppx_deriving.lookup name with
      | Some deriver ->
        items @ ((fn deriver) ~options ~path:(!path) typ_decls)
      | None ->
        if is_optional then items
        else raise_errorf ~loc "Cannot locate deriver %s" name)
    [item] deriver_exprs

let module_from_input_name () =
  match !Location.input_name with
  | "//toplevel//" -> []
  | filename -> [String.capitalize (Filename.(basename (chop_suffix filename ".ml")))]

let mapper argv =
  get_plugins () |> List.iter dynlink;
  add_plugins argv;
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
        | Some { Ppx_deriving.core_type = Some deriver } -> deriver
        | Some _ -> raise_errorf ~loc "Deriver %s does not support inline notation" name
        | None -> raise_errorf ~loc "Cannot locate deriver %s" name
      in
      begin match payload with
      | PTyp typ -> deriver typ
      | _ -> raise_errorf ~loc "Unrecognized [%%derive.*] syntax"
      end
    | { pexp_desc = Pexp_extension ({ txt = name; loc }, PTyp typ) } ->
      begin match Ppx_deriving.lookup name with
      | Some { Ppx_deriving.core_type = Some deriver } ->
        Ast_helper.with_default_loc typ.ptyp_loc (fun () -> deriver typ)
      | _ -> default_mapper.expr mapper expr
      end
    | _ -> default_mapper.expr mapper expr
  in
  let rec structure mapper items =
    match items with
    | [%stri [@@@findlib.ppxopt [%e? { pexp_desc = Pexp_tuple (
          [%expr "ppx_deriving"] :: elems) }]]] :: rest ->
      elems |>
        List.map (fun elem ->
          match elem with
          | { pexp_desc = Pexp_constant (Const_string (file, None))} -> file
          | _ -> assert false) |>
        add_plugins;
        structure mapper rest
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

