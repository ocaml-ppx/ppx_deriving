open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let prefix = "ord"
let raise_errorf = Ppx_deriving.raise_errorf

let () =
  Ppx_deriving.register "Ord" (fun options type_decls ->
    assert false)
