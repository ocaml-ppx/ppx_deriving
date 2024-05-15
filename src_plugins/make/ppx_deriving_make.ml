open Ppxlib
open Asttypes
open Parsetree
open Ast_helper
open Ppx_deriving.Ast_convenience

let deriver = "make"
let raise_errorf = Ppx_deriving.raise_errorf

let attr_default context = Attribute.declare "deriving.make.default" context
  Ast_pattern.(single_expr_payload __) (fun e -> e)
let attr_default = (attr_default Attribute.Context.label_declaration, attr_default Attribute.Context.core_type)

let mk_attr_split context = Attribute.declare_flag "deriving.make.split" context
let ct_attr_split = mk_attr_split Attribute.Context.core_type
let label_attr_split = mk_attr_split Attribute.Context.label_declaration

let attr_split = (label_attr_split, ct_attr_split)

let mk_attr_main context = Attribute.declare_flag "deriving.make.main" context
let ct_attr_main = mk_attr_main Attribute.Context.core_type
let label_attr_main = mk_attr_main Attribute.Context.label_declaration

let attr_main = (label_attr_main, ct_attr_main)

let get_label_attribute (label_attr, ct_attr) label =
  match Attribute.get label_attr label with
  | Some _ as v -> v
  | None -> Attribute.get ct_attr label.pld_type

let has_label_flag (label_flag, ct_flag) ({pld_type; _} as label) =
  Attribute.has_flag ct_flag pld_type || Attribute.has_flag label_flag label

let find_main labels =
  let mains, regulars = List.partition (has_label_flag attr_main) labels in
  match mains, regulars with
  | [], labels -> Ok (None, labels)
  | [main], labels -> Ok (Some main, labels)
  | _::{pld_loc; _}::_ , _ ->
    Error
      (Location.error_extensionf ~loc:pld_loc
         "Duplicate [@deriving.%s.main] annotation" deriver)

let is_optional ({ pld_name = { txt = name }; pld_type; _ } as label) =
  match get_label_attribute attr_default label with
  | Some _ -> true
  | None ->
    has_label_flag attr_split label ||
    (match Ppx_deriving.remove_pervasives ~deriver pld_type with
     | [%type: [%t? _] list]
     | [%type: [%t? _] option] -> true
     | _ -> false)

let add_str_label_arg ~quoter ~loc accum
    ({pld_name = {txt = name}; pld_type; _} as label) =
  match get_label_attribute attr_default label with
  | Some default ->
    Exp.fun_ (Label.optional name) (Some (Ppx_deriving.quote ~quoter default))
      (pvar name) accum
  | None ->
    let pld_type = Ppx_deriving.remove_pervasives ~deriver pld_type in
    if has_label_flag attr_split label then
      match pld_type with
      | [%type: [%t? lhs] * [%t? rhs] list] when name.[String.length name - 1] = 's' ->
        let name' = String.sub name 0 (String.length name - 1) in
        Exp.fun_ (Label.labelled name') None (pvar name')
          (Exp.fun_ (Label.optional name) (Some [%expr []]) (pvar name)
             [%expr let [%p pvar name] = [%e evar name'], [%e evar name] in [%e accum]])
      | _ ->
        Ast_builder.Default.pexp_extension ~loc
          (Location.error_extensionf ~loc
             "[@deriving.%s.split] annotation requires a type of form \
              'a * 'a list and label name ending with `s'"
             deriver)
    else
      match pld_type with
      | [%type: [%t? _] list] ->
        Exp.fun_ (Label.optional name) (Some [%expr []]) (pvar name) accum
      | [%type: [%t? _] option] ->
        Exp.fun_ (Label.optional name) None (pvar name) accum
      | _ -> Exp.fun_ (Label.labelled name) None (pvar name) accum

let str_of_record_type ~quoter ~loc labels =
  let fields =
    labels |> List.map (fun { pld_name = { txt = name; loc } } ->
        name, evar name) in
  match find_main labels with
  | Error extension -> Ast_builder.Default.pexp_extension ~loc extension
  | Ok (main, labels) ->
    let has_option = List.exists is_optional labels in
    let fn =
      match main with
      | Some { pld_name = { txt = name }} ->
        Exp.fun_ Label.nolabel None (pvar name) (record fields)
      | None when has_option ->
        Exp.fun_ Label.nolabel None (punit ()) (record fields)
      | None ->
        record fields
    in
    (* The labels list must be reversed here so that the arguments are in the
       same order as the record fields. *)
    List.fold_left (add_str_label_arg ~quoter ~loc) fn (List.rev labels)

let str_of_type ({ ptype_loc = loc } as type_decl) =
  let quoter = Ppx_deriving.create_quoter () in
  match type_decl.ptype_kind with
  | Ptype_record labels ->
    let creator = str_of_record_type ~quoter ~loc labels in
    Ok
      (Vb.mk (pvar (Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl))
         (Ppx_deriving.sanitize ~quoter creator))
  | _ ->
    Error
      (Location.error_extensionf ~loc
         "%s can be derived only for record types" deriver)

let wrap_predef_option typ =
  typ

let add_sig_label_arg accum
    ({pld_name = {txt = name; loc}; pld_type; _} as label) = 
  match get_label_attribute attr_default label with
  | Some _ ->
    Typ.arrow (Label.optional name) (wrap_predef_option pld_type) accum
  | None ->
    let pld_type = Ppx_deriving.remove_pervasives ~deriver pld_type in
    if has_label_flag attr_split label then
      match pld_type with
      | [%type: [%t? lhs] * [%t? rhs] list]
        when name.[String.length name - 1] = 's' ->
        let name' = String.sub name 0 (String.length name - 1) in
        Typ.arrow (Label.labelled name') lhs
          (Typ.arrow (Label.optional name)
             (wrap_predef_option [%type: [%t rhs] list]) accum)
      | _ ->
        Ast_builder.Default.ptyp_extension ~loc
          (Location.error_extensionf ~loc
             "[@deriving.%s.split] annotation requires a type of form \
              'a * 'a list and label name ending with `s'"
             deriver)
    else
      match pld_type with
      | [%type: [%t? _] list] ->
        Typ.arrow (Label.optional name) (wrap_predef_option pld_type) accum
      | [%type: [%t? opt] option] ->
        Typ.arrow (Label.optional name) (wrap_predef_option opt) accum
      | _ -> Typ.arrow (Label.labelled name) pld_type accum

let sig_of_record_type ~loc ~typ labels =
  match find_main labels with
  | Error extension -> Ast_builder.Default.ptyp_extension ~loc extension
  | Ok (main, labels) ->
    let has_option = List.exists is_optional labels in
    let typ =
      match main with
      | Some { pld_name = { txt = name }; pld_type } ->
        Typ.arrow Label.nolabel pld_type typ
      | None when has_option -> Typ.arrow Label.nolabel (tconstr "unit" []) typ
      | None -> typ
    in
    (* The labels list must be reversed here so that the arguments are in the
       same order as the record fields. *)
    List.fold_left add_sig_label_arg typ (List.rev labels)

let sig_of_type ({ ptype_loc = loc } as type_decl) =
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  match type_decl.ptype_kind with
  | Ptype_record labels ->
    let typ = sig_of_record_type ~loc ~typ labels in
    let val_name = Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl in
    Ok (Sig.value (Val.mk (mknoloc val_name) typ))
  | _ ->
    Error
      (Location.error_extensionf ~loc
         "%s can only be derived for record types" deriver)

(* Ppxlib does not keep track of which type the attribute was attached to
   in a set of type declarations and does not provide a nice and reliable
   way to manually check it.
   Until we have something better, we have to assume that the
   [[@@deriving make]] attribute was meant for the whole set and properly
   placed. That means that if there is at least one type declaration in the
   set for which we can derive make, we will ignore errors from the rest. *)

let partition_result l =
  let errors, oks =
    List.fold_left
      (fun (errors, oks) res ->
         match res with
         | Ok x -> (errors, x :: oks)
         | Error e -> (e :: errors, oks))
      ([], [])
      l
  in
  List.rev errors, List.rev oks

let impl_generator =
  Deriving.Generator.V2.make_noarg (fun ~ctxt (_, type_decls) ->
      match partition_result (List.map str_of_type type_decls) with
      | _, (_::_ as vbs) -> [Str.value Nonrecursive vbs]
      | errors, [] ->
        let loc = Expansion_context.Deriver.derived_item_loc ctxt in
        List.map (fun ext -> Ast_builder.Default.pstr_extension ~loc ext [])
          errors)

let intf_generator =
  Deriving.Generator.V2.make_noarg (fun ~ctxt (_, type_decls) ->
      match partition_result (List.map sig_of_type type_decls) with
      | _, (_::_ as vds) -> vds
      | errors, [] ->
        let loc = Expansion_context.Deriver.derived_item_loc ctxt in
        List.map (fun ext -> Ast_builder.Default.psig_extension ~loc ext [])
          errors)

let deriving: Deriving.t =
  Deriving.add
    deriver
    ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
