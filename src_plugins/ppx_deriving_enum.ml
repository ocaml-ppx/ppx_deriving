open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let prefix = "enum"
let raise_errorf = Ppx_deriving.raise_errorf

let () =
  Ppx_deriving.register "Enum" (fun ~options ~path type_decls ->
    let expr_of_type ({ ptype_name = { txt = name }; ptype_loc = loc } as type_decl) =
      let kind, mappings = Enumerable.mappings_of_type_decl ~prefix ~name:"Enum" type_decl in
      let patt name =
        match kind with
        | `Regular -> Pat.construct (mknoloc (Lident name)) None
        | `Polymorphic -> Pat.variant name None
      and expr name =
        match kind with
        | `Regular -> Exp.construct (mknoloc (Lident name)) None
        | `Polymorphic -> Exp.variant name None
      in
      let to_enum_cases =
        List.map (fun (value, { txt = name }) ->
          Exp.case (patt name) (int value)) mappings
      and from_enum_cases =
        List.map (fun (value, { txt = name }) ->
          Exp.case (pint value) (constr "Some" [expr name])) mappings @
        [Exp.case (Pat.any ()) (constr "None" [])]
      in
      [Vb.mk (pvar (Ppx_deriving.mangle_type_decl (`Suffix "to_enum") type_decl))
             (Exp.function_ to_enum_cases);
       Vb.mk (pvar (Ppx_deriving.mangle_type_decl (`Suffix "of_enum") type_decl))
             (Exp.function_ from_enum_cases)]
    in
    let sig_of_type type_decl =
      let typ = Ppx_deriving.core_type_of_type_decl type_decl in
      [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Suffix "to_enum") type_decl))
                 [%type: [%t typ] -> int]);
       Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Suffix "of_enum") type_decl))
                 [%type: int -> [%t typ] option])]
    in
    Ppx_deriving.catch (fun () ->
      [Str.value Recursive (List.concat (List.map expr_of_type type_decls))]),
    List.concat (List.map sig_of_type type_decls))
