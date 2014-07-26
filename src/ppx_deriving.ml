open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

type deriver = options:(string * expression) list ->
               path:string list ->
               type_declaration list -> structure * signature

let registry : (string, deriver) Hashtbl.t
             = Hashtbl.create 16

let register = Hashtbl.add registry

let lookup name =
  try  Some (Hashtbl.find registry name)
  with Not_found -> None

let raise_errorf ?sub ?if_highlight ?loc message =
  message |> Printf.kprintf (fun str ->
    let err = Location.error ?sub ?if_highlight ?loc str in
    raise (Location.Error err))

let catch f =
  try f ()
  with exn ->
    match Location.error_of_exn exn with
    | Some error -> [Str.extension (Ast_mapper.extension_of_error error)]
    | None -> raise exn

let string_of_core_type typ =
  Format.asprintf "%a" Pprintast.core_type { typ with ptyp_attributes = [] }

module Arg = struct
  let int expr =
    match expr with
    | { pexp_desc = Pexp_constant (Const_int n) } -> `Ok n
    | _ -> `Error "integer"

  let string expr =
    match expr with
    | { pexp_desc = Pexp_constant (Const_string (n, None)) } -> `Ok n
    | _ -> `Error "string"

  let enum values expr =
    match expr with
    | { pexp_desc = Pexp_variant (name, None) }
      when List.mem name values -> `Ok name
    | _ -> `Error (Printf.sprintf "one of: %s"
                    (String.concat ", " (List.map (fun s -> "`"^s) values)))

  let payload ~name conv attr =
    match attr with
    | None -> None
    | Some ({ txt = attr_name }, PStr [{ pstr_desc = Pstr_eval (expr, []) }]) ->
      begin match conv expr with
      | `Ok v -> Some v
      | `Error desc ->
        raise_errorf ~loc:expr.pexp_loc "%s: invalid [@%s]: %s expected" name attr_name desc
      end
    | Some ({ txt = attr_name; loc }, _) ->
      raise_errorf ~loc "%s: invalid [@%s]: value expected" name attr_name
end

let expand_path ~path ident =
  String.concat "." (path @ [ident])

let path_of_type_decl ~path type_decl =
  match type_decl.ptype_manifest with
  | Some { ptyp_desc = Ptyp_constr ({ txt = lid }, _) } ->
    begin match lid with
    | Lident _ -> []
    | Ldot (lid, _) -> Longident.flatten lid
    | Lapply _ -> assert false
    end
  | _ -> path

let mangle_lid ?(prefix="") ?(suffix="") lid =
  match lid with
  | Lident s    -> Lident (prefix ^ s ^ suffix)
  | Ldot (p, s) -> Ldot (p, prefix ^ s ^ suffix)
  | Lapply _    -> assert false

let attr ~prefix name attrs =
  let starts str prefix =
    String.length str >= String.length prefix &&
      String.sub str 0 (String.length prefix) = prefix
  in
  let try_prefix prefix f =
    if List.exists (fun ({ txt }, _) -> starts txt prefix) attrs
    then prefix
    else f ()
  in
  let name =
    try_prefix ("deriving."^prefix^".") (fun () ->
      try_prefix (prefix^".") (fun () ->
        name))
  in
  try Some (List.find (fun ({ txt }, _) -> txt = name) attrs)
  with Not_found -> None

let fold_type fn accum { ptype_params }=
  List.fold_left (fun accum (param, _) ->
      match param with
      | { ptyp_desc = Ptyp_any } -> accum
      | { ptyp_desc = Ptyp_var name } ->
        fn accum name
      | _ -> assert false)
    accum ptype_params

let poly_fun_of_type_decl type_decl expr =
  fold_type (fun expr name -> Exp.fun_ "" None (pvar ("poly_"^name)) expr) expr type_decl

let poly_apply_of_type_decl type_decl expr =
  fold_type (fun expr name -> Exp.apply expr ["", evar ("poly_"^name)]) expr type_decl

let poly_arrow_of_type_decl fn type_decl typ =
  fold_type (fun typ name -> Typ.arrow "" (fn (Typ.var name)) typ) typ type_decl

let core_type_of_type_decl { ptype_name = { txt = name }; ptype_params } =
  Typ.constr (mknoloc (Lident name)) (List.map fst ptype_params)

let fold_exprs ?unit fn exprs =
  match exprs with
  | [] -> (match unit with Some x -> x | None -> assert false)
  | [a] -> a
  | hd::tl -> List.fold_left fn hd tl

let seq_reduce x a b =
  [%expr [%e a]; [%e x]; [%e b]]

let binop_reduce x a b =
  [%expr [%e x] [%e a] [%e b]]
