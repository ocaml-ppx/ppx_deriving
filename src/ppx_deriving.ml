open Longident
open Parsetree

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
