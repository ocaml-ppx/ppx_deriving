open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let deriver = "lens"
let raise_errorf = Ppx_deriving.raise_errorf

let parse_options options =
  options |> List.iter (fun (name, expr) ->
    match name with
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name)

(* builds the expression: { record with field = value } *)
let updated_record record field value =
  Exp.mk (
    Pexp_record (
      [ (mknoloc (Lident field), Exp.mk (Pexp_ident (mknoloc (Lident value)))) ],
      Some (Exp.mk (Pexp_ident (mknoloc (Lident record))))
    )
  )

let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  ignore (parse_options options);
  match type_decl.ptype_kind with
  | Ptype_record labels -> labels
    |> List.map (fun { pld_name = { txt = name; loc } } -> 
      name, [%expr Lens.{
        get = (fun r -> [%e Exp.field (evar "r") (mknoloc (Lident name))] );
        set = (fun v r -> [%e updated_record "r" name "v"]);
      }]
    )
    |> List.map (fun (name,lens) ->
      Vb.mk (pvar (Ppx_deriving.mangle_type_decl (`Suffix name) type_decl)) lens
    )
  | _ -> raise_errorf ~loc "%s can be derived only for record types" deriver

let type_named name =
  Typ.mk (Ptyp_constr (mknoloc (Lident name), []))

let sig_of_type ~options ~path ({ ptype_loc = loc; ptype_name = { txt = record_name } } as type_decl) =
  ignore (parse_options options);
  match type_decl.ptype_kind with
  | Ptype_record labels -> labels
    |> List.map (fun { pld_name = { txt = name; loc }; pld_type } -> 
      let lens_type = [%type: ([%t type_named record_name], [%t pld_type]) Lens.t] in
      Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Suffix name) type_decl)) lens_type)
    )
  | _ -> raise_errorf ~loc "%s can be derived only for record types" deriver

let () =
  Ppx_deriving.(register (create deriver
    ~type_decl_str: (fun ~options ~path type_decls ->
       [Str.value Nonrecursive (List.concat (List.map (str_of_type ~options ~path) type_decls))])
    ~type_decl_sig: (fun ~options ~path type_decls ->
       List.concat (List.map (sig_of_type ~options ~path) type_decls))
    ()
  ))
