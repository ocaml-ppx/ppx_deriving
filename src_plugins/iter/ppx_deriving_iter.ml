open Ppxlib
open Asttypes
open Parsetree
open Ast_helper
open Ppx_deriving.Ast_convenience

let deriver = "iter"
let raise_errorf = Ppx_deriving.raise_errorf

let ct_attr_nobuiltin = Attribute.declare_flag "deriving.iter.nobuiltin" Attribute.Context.core_type

let argn = Printf.sprintf "a%d"
let argl = Printf.sprintf "a%s"

let pattn typs   = List.mapi (fun i _ -> pvar (argn i)) typs
let pattl labels = List.map (fun { pld_name = { txt = n } } -> n, pvar (argl n)) labels

let pconstrrec name fields = pconstr name [precord ~closed:Closed fields]

let rec expr_of_typ typ =
  let loc = !Ast_helper.default_loc in
  let typ = Ppx_deriving.remove_pervasives ~deriver typ in
  match typ with
  | _ when Ppx_deriving.free_vars_in_core_type typ = [] -> [%expr fun _ -> ()]
  | { ptyp_desc = Ptyp_constr _ } ->
    let builtin = not (Attribute.has_flag ct_attr_nobuiltin typ) in
    begin match builtin, typ with
    | true, [%type: [%t? typ] ref] ->
      [%expr fun x -> [%e expr_of_typ typ] !x]
    | true, [%type: [%t? typ] list] ->
      [%expr Ppx_deriving_runtime.List.iter [%e expr_of_typ typ]]
    | true, [%type: [%t? typ] array] ->
      [%expr Ppx_deriving_runtime.Array.iter [%e expr_of_typ typ]]
    | true, [%type: [%t? typ] option] ->
      [%expr function None -> () | Some x -> [%e expr_of_typ typ] x]
    | true, [%type: ([%t? ok_t], [%t? err_t]) result]
    | true, [%type: ([%t? ok_t], [%t? err_t]) Result.result] ->
      [%expr
        function
        | Ok ok -> ignore ([%e expr_of_typ ok_t] ok)
        | Error err -> ignore ([%e expr_of_typ err_t] err)]
    | _, { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
      app (Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix deriver) lid)))
          (List.map expr_of_typ args)
    | _ -> assert false
    end
  | { ptyp_desc = Ptyp_tuple typs } ->
    [%expr fun [%p ptuple (List.mapi (fun i _ -> pvar (argn i)) typs)] ->
      [%e Ppx_deriving.(fold_exprs seq_reduce
            (List.mapi (fun i typ -> app (expr_of_typ typ) [evar (argn i)]) typs))]];
  | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
    let cases =
      fields |> List.map (fun field ->
        let variant label popt =
          Pat.variant label.txt popt
        in
        match field.prf_desc with
        | Rtag(label, true (*empty*), []) ->
          Exp.case (variant label None) [%expr ()]
        | Rtag(label, false, [typ]) ->
          Exp.case (variant label (Some [%pat? x]))
                   [%expr [%e expr_of_typ typ] x]
        | Rinherit({ ptyp_desc = Ptyp_constr (tname, _) } as typ) ->
          Exp.case [%pat? [%p Pat.type_ tname] as x]
                   [%expr [%e expr_of_typ typ] x]
        | _ ->
          raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                       deriver (Ppx_deriving.string_of_core_type typ))
    in
    Exp.function_ cases
  | { ptyp_desc = Ptyp_var name } -> [%expr ([%e evar ("poly_"^name)] : [%t Typ.var name] -> unit)]
  | { ptyp_desc = Ptyp_alias (typ, name) } ->
    [%expr fun x -> [%e evar ("poly_"^name.txt)] x; [%e expr_of_typ typ] x]
  | { ptyp_loc } ->
    raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                 deriver (Ppx_deriving.string_of_core_type typ)

and expr_of_label_decl { pld_type; pld_attributes } =
  let attrs = pld_type.ptyp_attributes @ pld_attributes in
  expr_of_typ { pld_type with ptyp_attributes = attrs }

let str_of_type ({ ptype_loc = loc } as type_decl) =
  let iterator =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest -> expr_of_typ manifest
    | Ptype_variant constrs, _ ->
      constrs |>
      List.map (fun { pcd_name = { txt = name' }; pcd_args } ->
        match pcd_args with
        | Pcstr_tuple(typs) ->
          let args = List.mapi (fun i typ -> app (expr_of_typ typ) [evar (argn i)]) typs in
          let result =
            match args with
            | []   -> [%expr ()]
            | args -> Ppx_deriving.(fold_exprs seq_reduce) args
          in
          Exp.case (pconstr name' (pattn typs)) result
        | Pcstr_record(labels) ->
          let args = labels |> List.map (fun ({ pld_name = { txt = n }; _ } as pld) ->
                        [%expr [%e expr_of_label_decl pld] [%e evar (argl n)]]) in
          Exp.case (pconstrrec name' (pattl labels))
                   (Ppx_deriving.(fold_exprs seq_reduce) args)
        ) |>
      Exp.function_
    | Ptype_record labels, _ ->
      let fields =
        labels |> List.mapi (fun i ({ pld_name = { txt = name }; _ } as pld) ->
          [%expr [%e expr_of_label_decl pld]
              [%e Exp.field (evar "x") (mknoloc (Lident name))]])
      in
      [%expr fun x -> [%e Ppx_deriving.(fold_exprs seq_reduce) fields]]
    | Ptype_abstract, None ->
      raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    | Ptype_open, _        ->
      raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  [Vb.mk ~attrs:[Ppx_deriving.attr_warning [%expr "-39"]]
         (pvar (Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl))
         (polymorphize iterator)]

let sig_of_type type_decl =
  let loc = !Ast_helper.default_loc in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let polymorphize = Ppx_deriving.poly_arrow_of_type_decl
                        (fun var -> [%type: [%t var] -> Ppx_deriving_runtime.unit]) type_decl in
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl))
              (polymorphize [%type: [%t typ] -> Ppx_deriving_runtime.unit]))]

let impl_generator = Deriving.Generator.V2.make_noarg (fun ~ctxt:_ (_, type_decls) ->
  [Str.value Recursive (List.concat (List.map str_of_type type_decls))])

let intf_generator = Deriving.Generator.V2.make_noarg (fun ~ctxt:_ (_, type_decls) ->
  List.concat (List.map sig_of_type type_decls))

let deriving: Deriving.t =
  Deriving.add
    deriver
    ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
