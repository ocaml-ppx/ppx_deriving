open Longident
open Location
open Parsetree
open Ast_helper
open Ast_convenience

type deriver = (string * expression) list -> type_declaration list -> structure * signature

let registry : (string, deriver) Hashtbl.t
             = Hashtbl.create 16

let register = Hashtbl.add registry

let lookup name =
  try  Some (Hashtbl.find registry name)
  with Not_found -> None

let raise_errorf ?sub ?if_highlight ?loc message =
  Printf.kprintf (fun str -> raise (Location.Error (Location.error ?loc str))) message

let string_of_core_type ptyp =
  Format.asprintf "%a" Pprintast.core_type { ptyp with ptyp_attributes = [] }

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

let poly_arrow_of_type_decl ~fn type_decl typ =
  fold_type (fun typ name -> Typ.arrow "" (fn name) typ) typ type_decl

