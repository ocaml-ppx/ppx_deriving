open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let prefix = "bounded"
let raise_errorf = Ppx_deriving.raise_errorf

let () =
  Ppx_deriving.register "Bounded" (fun ~options ~path type_decls ->
    let expr_of_type ({ ptype_name = { txt = name }; ptype_loc = loc } as type_decl) =
      let _, mappings = Enumerable.mappings_of_type_decl ~prefix ~name:"Bounded" type_decl in
      let indexes = List.map fst mappings in
      [Vb.mk (pvar (Ppx_deriving.mangle_type_decl (`Prefix "min") type_decl))
             (int (List.fold_left min max_int indexes));
       Vb.mk (pvar (Ppx_deriving.mangle_type_decl (`Prefix "max") type_decl))
             (int (List.fold_left max min_int indexes))]
    in
    let sig_of_type type_decl =
      [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "min") type_decl))
                 [%type: int]);
       Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "max") type_decl))
                 [%type: int])]
    in
    Ppx_deriving.catch (fun () ->
      [Str.value Nonrecursive (List.concat (List.map expr_of_type type_decls))]),
    List.concat (List.map sig_of_type type_decls))
