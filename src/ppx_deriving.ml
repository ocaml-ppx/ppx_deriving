open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

type deriver = {
  core_type : (core_type -> expression) option;
  structure : options:(string * expression) list -> path:string list ->
              type_declaration list -> structure;
  signature : options:(string * expression) list -> path:string list ->
              type_declaration list -> signature;
}

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

let string_of_core_type typ =
  Format.asprintf "%a" Pprintast.core_type { typ with ptyp_attributes = [] }

module Arg = struct
  let expr expr =
    `Ok expr

  let int expr =
    match expr with
    | { pexp_desc = Pexp_constant (Const_int n) } -> `Ok n
    | _ -> `Error "integer"

  let bool expr =
    match expr with
    | [%expr true] -> `Ok true
    | [%expr false] -> `Ok false
    | _ -> `Error "boolean"

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

  let get_attr ~deriver conv attr =
    match attr with
    | None -> None
    | Some ({ txt = name }, PStr [{ pstr_desc = Pstr_eval (expr, []) }]) ->
      begin match conv expr with
      | `Ok v -> Some v
      | `Error desc ->
        raise_errorf ~loc:expr.pexp_loc "%s: invalid [@%s]: %s expected" deriver name desc
      end
    | Some ({ txt = name; loc }, _) ->
      raise_errorf ~loc "%s: invalid [@%s]: value expected" deriver name

  let get_flag ~deriver attr =
    match attr with
    | None -> false
    | Some ({ txt = name }, PStr []) -> true
    | Some ({ txt = name; loc }, _) ->
      raise_errorf ~loc "%s: invalid [@%s]: empty structure expected" deriver name

  let get_expr ~deriver conv expr =
    match conv expr with
    | `Error desc -> raise_errorf ~loc:expr.pexp_loc "%s: %s expected" deriver desc
    | `Ok v -> v
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

let mangle ?(fixpoint="t") affix name =
  match name = fixpoint, affix with
  | true,  (`Prefix x | `Suffix x) -> x
  | false, `Prefix x -> x ^ "_" ^ name
  | false, `Suffix x -> name ^ "_" ^ x

let mangle_type_decl ?fixpoint affix { ptype_name = { txt = name } } =
  mangle ?fixpoint affix name

let mangle_lid ?fixpoint affix lid =
  match lid with
  | Lident s    -> Lident (mangle ?fixpoint affix s)
  | Ldot (p, s) -> Ldot (p, mangle ?fixpoint affix s)
  | Lapply _    -> assert false

let attr ~deriver name attrs =
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
    try_prefix ("deriving."^deriver^".") (fun () ->
      try_prefix (deriver^".") (fun () ->
        name))
  in
  try Some (List.find (fun ({ txt }, _) -> txt = name) attrs)
  with Not_found -> None

let fold_type_decl fn accum { ptype_params }=
  List.fold_left (fun accum (param, _) ->
      match param with
      | { ptyp_desc = Ptyp_any } -> accum
      | { ptyp_desc = Ptyp_var name } ->
        fn accum name
      | _ -> assert false)
    accum ptype_params

let free_vars_in_core_type typ =
  let rec free_in typ =
    match typ with
    | { ptyp_desc = Ptyp_any } -> []
    | { ptyp_desc = Ptyp_var name } -> [name]
    | { ptyp_desc = Ptyp_arrow (_, x, y) } -> free_in x @ free_in y
    | { ptyp_desc = (Ptyp_tuple xs | Ptyp_constr (_, xs)) } ->
      List.map free_in xs |> List.concat
    | { ptyp_desc = Ptyp_alias (x, name) } -> [name] @ free_in x
    | { ptyp_desc = Ptyp_poly (bound, x) } ->
      List.filter (fun y -> not (List.mem y bound)) (free_in x)
    | _ -> assert false
  in
  let rec uniq acc lst =
    match lst with
    | a :: b :: lst when a = b -> uniq acc (b :: lst)
    | x :: lst -> uniq (x :: acc) lst
    | [] -> acc
  in
  List.rev (uniq [] (free_in typ))

let var_name_of_int i =
  let letter = "abcdefghijklmnopqrstuvwxyz" in
  let rec loop i =
    if i < 26 then [letter.[i]] else letter.[i mod 26] :: loop (i / 26)
  in
  String.concat "" (List.map (String.make 1) (loop i))

let fresh_var bound =
  let rec loop i =
    let var_name = var_name_of_int i in
    if List.mem var_name bound then loop (i + 1) else var_name
  in
  loop 0

let poly_fun_of_type_decl type_decl expr =
  fold_type_decl (fun expr name -> Exp.fun_ "" None (pvar ("poly_"^name)) expr) expr type_decl

let poly_apply_of_type_decl type_decl expr =
  fold_type_decl (fun expr name -> Exp.apply expr ["", evar ("poly_"^name)]) expr type_decl

let poly_arrow_of_type_decl fn type_decl typ =
  fold_type_decl (fun typ name -> Typ.arrow "" (fn (Typ.var name)) typ) typ type_decl

let core_type_of_type_decl { ptype_name = { txt = name }; ptype_params } =
  Typ.constr (mknoloc (Lident name)) (List.map fst ptype_params)

let fold_exprs ?unit fn exprs =
  match exprs with
  | [] -> (match unit with Some x -> x | None -> assert false)
  | [a] -> a
  | hd::tl -> List.fold_left fn hd tl

let seq_reduce ?sep a b =
  match sep with
  | Some x -> [%expr [%e a]; [%e x]; [%e b]]
  | None -> [%expr [%e a]; [%e b]]

let binop_reduce x a b =
  [%expr [%e x] [%e a] [%e b]]
