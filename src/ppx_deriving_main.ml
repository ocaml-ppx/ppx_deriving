open Asttypes
open Parsetree
open Ast_helper

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

let mapper argv =
  get_plugins () |> List.iter dynlink;
  add_plugins argv;
  let structure mapper = function
    | [%stri [@@@findlib.ppxopt [%e? { pexp_desc = Pexp_tuple (
          [%expr "ppx_deriving"] :: elems) }]]] :: rest ->
      elems |>
        List.map (fun elem ->
          match elem with
          | { pexp_desc = Pexp_constant (Const_string (file, None))} -> file
          | _ -> assert false) |>
        add_plugins;
        mapper.Ast_mapper.structure mapper rest
    | items -> Ppx_deriving.mapper.Ast_mapper.structure mapper items in
  { Ppx_deriving.mapper with Ast_mapper.structure }

let () =
  Ast_mapper.register "ppx_deriving" mapper

