open Ppxlib
open Asttypes
open Parsetree
open Ast_helper
open Ppx_deriving.Ast_convenience

let deriver = "create"
let raise_errorf = Ppx_deriving.raise_errorf

let attr_default context = Attribute.declare "deriving.create.default" context
  Ast_pattern.(single_expr_payload __) (fun e -> e)
let attr_default = (attr_default Attribute.Context.label_declaration, attr_default Attribute.Context.core_type)

let attr_split context = Attribute.declare_flag "deriving.create.split" context
let ct_attr_split = attr_split Attribute.Context.core_type
let label_attr_split = attr_split Attribute.Context.label_declaration

let attr_main context = Attribute.declare_flag "deriving.create.main" context
let ct_attr_main = attr_main Attribute.Context.core_type
let label_attr_main = attr_main Attribute.Context.label_declaration

let get_label_attribute (label_attr, ct_attr) label =
  match Attribute.get label_attr label with
  | Some _ as v -> v
  | None -> Attribute.get ct_attr label.pld_type

let find_main labels =
  List.fold_left (fun (main, labels) ({ pld_type; pld_loc; pld_attributes } as label) ->
    if Attribute.has_flag ct_attr_main pld_type || Attribute.has_flag label_attr_main label then
      match main with
      | Some _ -> raise_errorf ~loc:pld_loc "Duplicate [@deriving.%s.main] annotation" deriver
      | None -> Some label, labels
    else
      main, label :: labels)
    (None, []) labels

let str_of_type ({ ptype_loc = loc } as type_decl) =
  let quoter = Ppx_deriving.create_quoter () in
  let creator =
    match type_decl.ptype_kind with
    | Ptype_record labels ->
      let fields =
        labels |> List.map (fun { pld_name = { txt = name; loc } } ->
          name, evar name) in
      let main, labels = find_main labels in
      let fn =
        match main with
        | Some { pld_name = { txt = name }} ->
          Exp.fun_ Label.nolabel None (pvar name) (record fields)
        | None ->
          Exp.fun_ Label.nolabel None (punit ()) (record fields)
      in
      List.fold_left (fun accum ({ pld_name = { txt = name }; pld_type; pld_attributes } as label) ->
        match get_label_attribute attr_default label with
        | Some default -> Exp.fun_ (Label.optional name) (Some (Ppx_deriving.quote ~quoter default))
                                   (pvar name) accum
        | None ->
        let pld_type = Ppx_deriving.remove_pervasives ~deriver pld_type in
        if Attribute.has_flag label_attr_split label || Attribute.has_flag ct_attr_split pld_type then
          match pld_type with
          | [%type: [%t? lhs] * [%t? rhs] list] when name.[String.length name - 1] = 's' ->
            let name' = String.sub name 0 (String.length name - 1) in
            Exp.fun_ (Label.labelled name') None (pvar name')
              (Exp.fun_ (Label.optional name) (Some [%expr []]) (pvar name)
                [%expr let [%p pvar name] = [%e evar name'], [%e evar name] in [%e accum]])
          | _ -> raise_errorf ~loc "[@deriving.%s.split] annotation requires a type of form \
                                    'a * 'b list and label name ending with `s'" deriver
        else
          match pld_type with
          | [%type: [%t? _] list] ->
            Exp.fun_ (Label.optional name) (Some [%expr []]) (pvar name) accum
          | [%type: [%t? _] option] ->
            Exp.fun_ (Label.optional name) None (pvar name) accum
          | _ -> Exp.fun_ (Label.labelled name) None (pvar name) accum)
          fn labels
    | _ -> raise_errorf ~loc "%s can be derived only for record types" deriver
  in
  [Vb.mk (pvar (Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl))
         (Ppx_deriving.sanitize ~quoter creator)]

let wrap_predef_option typ =
  typ

let sig_of_type ({ ptype_loc = loc } as type_decl) =
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let typ =
    match type_decl.ptype_kind with
    | Ptype_record labels ->
      let main, labels = find_main labels in
      let typ =
        match main with
        | Some { pld_name = { txt = name }; pld_type } ->
          Typ.arrow Label.nolabel pld_type typ
        | None ->
          Typ.arrow Label.nolabel (tconstr "unit" []) typ
      in
      List.fold_left (fun accum ({ pld_name = { txt = name; loc }; pld_type; pld_attributes } as label) ->
        match get_label_attribute attr_default label with
        | Some _ -> Typ.arrow (Label.optional name) (wrap_predef_option pld_type) accum
        | None ->
        let pld_type = Ppx_deriving.remove_pervasives ~deriver pld_type in
        if Attribute.has_flag ct_attr_split pld_type || Attribute.has_flag label_attr_split label then
          match pld_type with
          | [%type: [%t? lhs] * [%t? rhs] list] when name.[String.length name - 1] = 's' ->
            let name' = String.sub name 0 (String.length name - 1) in
            Typ.arrow (Label.labelled name') lhs
              (Typ.arrow (Label.optional name) (wrap_predef_option [%type: [%t rhs] list]) accum)
          | _ -> raise_errorf ~loc "[@deriving.%s.split] annotation requires a type of form \
                                    'a * 'b list and label name ending with `s'" deriver
        else
          match pld_type with
          | [%type: [%t? _] list] ->
            Typ.arrow (Label.optional name) (wrap_predef_option pld_type) accum
          | [%type: [%t? opt] option] ->
            Typ.arrow (Label.optional name) (wrap_predef_option opt) accum
          | _ -> Typ.arrow (Label.labelled name) pld_type accum)
        typ labels
    | _ -> raise_errorf ~loc "%s can only be derived for record types" deriver
  in
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl)) typ)]

let impl_generator = Deriving.Generator.V2.make_noarg (fun ~ctxt:_ (_, type_decls) ->
  [Str.value Nonrecursive (List.concat (List.map str_of_type type_decls))])

let intf_generator = Deriving.Generator.V2.make_noarg (fun ~ctxt:_ (_, type_decls) ->
  List.concat (List.map sig_of_type type_decls))

let deriving: Deriving.t =
  Deriving.add
    deriver
    ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
