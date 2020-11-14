open Ppxlib
open Asttypes
open Parsetree
open Ast_helper

module Ast_mapper = Ocaml_common.Ast_mapper

module From_current =
  Ppxlib_ast.Selected_ast.Of_ocaml

module To_current =
  Ppxlib_ast.Selected_ast.To_ocaml

let raise_errorf = Ppx_deriving.raise_errorf

let dynlink ?(loc=Location.none) filename =
  let filename = Dynlink.adapt_filename filename in
  try
    Dynlink.loadfile filename
  with Dynlink.Error error ->
    raise_errorf ~loc "Cannot load %s: %s" filename (Dynlink.error_message error)

let init_findlib = lazy (
  Findlib.init ();
  Findlib.record_package Findlib.Record_core "ppx_deriving.api";
)

let load_ocamlfind_package ?loc pkg =
  Lazy.force init_findlib;
  try
    Fl_dynload.load_packages [pkg]
  with Dynlink.Error error ->
    raise_errorf ?loc "Cannot load %s: %s" pkg (Dynlink.error_message error)

let load_plugin ?loc plugin =
  let len = String.length plugin in
  let pkg_prefix = "package:" in
  let pkg_prefix_len = String.length pkg_prefix in
  if len >= pkg_prefix_len &&
     String.sub plugin 0 pkg_prefix_len = pkg_prefix then
    let pkg = String.sub plugin pkg_prefix_len (len - pkg_prefix_len) in
    load_ocamlfind_package ?loc pkg
  else
    dynlink ?loc plugin

let get_plugins () =
  match Ast_mapper.get_cookie "ppx_deriving" with
  | None -> []
  | Some expr ->
      match From_current.copy_expression expr with
      | { pexp_desc = Pexp_tuple exprs } ->
        exprs |> List.map (fun expr ->
          match expr with
          | { pexp_desc = Pexp_constant (Pconst_string (file, _, None)) } -> file
          | _ -> assert false)
      | _ -> assert false

let add_plugins plugins =
  let loaded  = get_plugins () in
  let plugins = List.filter (fun file -> not (List.mem file loaded)) plugins in
  List.iter load_plugin plugins;
  let loaded  = loaded @ plugins in
  Ast_mapper.set_cookie "ppx_deriving"
    (To_current.copy_expression
       (Exp.tuple (List.map (Ast_builder.Default.estring ~loc:Location.none) loaded)))

let mapper argv =
  get_plugins () |> List.iter load_plugin;
  add_plugins argv;
  let module Current_ast = Ppxlib_ast.Selected_ast in
  let structure s =
    match s with
    | [] -> []
    | hd :: tl ->
        match
          hd
        with
        | ([%stri [@@@findlib.ppxopt [%e? { pexp_desc = Pexp_tuple (
            [%expr "ppx_deriving"] :: elems) }]]]) ->
            elems |>
            List.map (fun elem ->
              match elem with
              | { pexp_desc = Pexp_constant (Pconst_string (file, _, None))} ->
                  file
              | _ -> assert false) |>
            add_plugins;
            Ppxlib.Driver.map_structure tl
        | _ -> Ppxlib.Driver.map_structure s
  in
  let structure _ st =
    Current_ast.of_ocaml Structure st
    |> structure
    |> Current_ast.to_ocaml Structure
  in
  let signature _ si =
    Current_ast.of_ocaml Signature si
    |> Ppxlib.Driver.map_signature
    |> Current_ast.to_ocaml Signature
  in
  { Ast_mapper.default_mapper with structure; signature }

let () =
  Ast_mapper.register "ppx_deriving" mapper
