open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let deriver = "create"
let raise_errorf = Ppx_deriving.raise_errorf

let parse_options options =
  options |> List.iter (fun (name, expr) ->
    match name with
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name)

let attr_default attrs =
  Ppx_deriving.(attrs |> attr ~deriver "default" |> Arg.(get_attr ~deriver expr))

let attr_split attrs =
  Ppx_deriving.(attrs |> attr ~deriver "split" |> Arg.get_flag ~deriver)

let find_main labels =
  List.fold_left (fun (main, labels) ({ pld_type; pld_loc; pld_attributes } as label) ->
    if Ppx_deriving.(pld_type.ptyp_attributes @ pld_attributes |>
                     attr ~deriver "main" |> Arg.get_flag ~deriver) then
      match main with
      | Some _ -> raise_errorf ~loc:pld_loc "Duplicate [@deriving.%s.main] annotation" deriver
      | None -> Some label, labels
    else
      main, label :: labels)
    (None, []) labels

let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  parse_options options;
  let mapper =
    match type_decl.ptype_kind with
    | Ptype_record labels ->
      let fields =
        labels |> List.map (fun { pld_name = { txt = name; loc } } ->
          name, evar name) in
      let main, labels = find_main labels in
      let fn =
        match main with
        | Some { pld_name = { txt = name }} ->
          Exp.fun_ "" None (pvar name) (record fields)
        | None ->
          Exp.fun_ "" None (punit ()) (record fields)
      in
      List.fold_left (fun accum { pld_name = { txt = name }; pld_type; pld_attributes } ->
        let attrs = pld_attributes @ pld_type.ptyp_attributes in
        match attr_default attrs with
        | Some default -> Exp.fun_ ("?"^name) (Some default) (pvar name) accum
        | None ->
        if attr_split attrs then
          match pld_type with
          | [%type: [%t? lhs] * [%t? rhs] list] when name.[String.length name - 1] = 's' ->
            let name' = String.sub name 0 (String.length name - 1) in
            Exp.fun_ name' None (pvar name')
              (Exp.fun_ ("?"^name) (Some [%expr []]) (pvar name)
                [%expr let [%p pvar name] = [%e evar name'], [%e evar name] in [%e accum]])
          | _ -> raise_errorf ~loc "[@deriving.%s.split] annotation requires a type of form \
                                    'a * 'b list and label name ending with `s'" deriver
        else
          match pld_type with
          | [%type: [%t? _] list] ->
            Exp.fun_ ("?"^name) (Some [%expr []]) (pvar name) accum
          | [%type: [%t? _] option] ->
            Exp.fun_ ("?"^name) None (pvar name) accum
          | _ -> Exp.fun_ name None (pvar name) accum)
          fn labels
    | _ -> raise_errorf ~loc "%s can be derived only for record types" deriver
  in
  [Vb.mk (pvar (Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl)) mapper]

let wrap_predef_option typ =
  let predef_option = mknoloc (Ldot (Lident "*predef*", "option")) in
  Typ.constr predef_option [typ]

let sig_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  parse_options options;
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let typ =
    match type_decl.ptype_kind with
    | Ptype_record labels ->
      let main, labels = find_main labels in
      let typ =
        match main with
        | Some { pld_name = { txt = name }; pld_type } ->
          Typ.arrow "" pld_type typ
        | None ->
          Typ.arrow "" (tconstr "unit" []) typ
      in
      List.fold_left (fun accum { pld_name = { txt = name; loc }; pld_type; pld_attributes } ->
        let attrs = pld_type.ptyp_attributes @ pld_attributes in
        match attr_default attrs with
        | Some _ -> Typ.arrow ("?"^name) (wrap_predef_option pld_type) accum
        | None ->
        if attr_split attrs then
          match pld_type with
          | [%type: [%t? lhs] * [%t? rhs] list] when name.[String.length name - 1] = 's' ->
            let name' = String.sub name 0 (String.length name - 1) in
            Typ.arrow name' lhs
              (Typ.arrow ("?"^name) (wrap_predef_option [%type: [%t rhs] list]) accum)
          | _ -> raise_errorf ~loc "[@deriving.%s.split] annotation requires a type of form \
                                    'a * 'b list and label name ending with `s'" deriver
        else
          match pld_type with
          | [%type: [%t? _] list] ->
            Typ.arrow ("?"^name) (wrap_predef_option pld_type) accum
          | [%type: [%t? opt] option] ->
            Typ.arrow ("?"^name) (wrap_predef_option opt) accum
          | _ -> Typ.arrow name pld_type accum)
        typ labels
    | _ -> raise_errorf ~loc "%s can only be derived for record types" deriver
  in
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl)) typ)]

let () =
  Ppx_deriving.(register (create deriver
    ~type_decl_str: (fun ~options ~path type_decls ->
       [Str.value Nonrecursive (List.concat (List.map (str_of_type ~options ~path) type_decls))])
    ~type_decl_sig: (fun ~options ~path type_decls ->
       List.concat (List.map (sig_of_type ~options ~path) type_decls))
    ()
  ))
