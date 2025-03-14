open Ppxlib
open Asttypes
open Parsetree
open Ast_helper
open Ppx_deriving.Ast_convenience

let deriver = "map"
let raise_errorf = Ppx_deriving.raise_errorf

let ct_attr_nobuiltin = Attribute.declare_flag "deriving.map.nobuiltin" Attribute.Context.core_type

let argn = Printf.sprintf "a%d"
let argl = Printf.sprintf "a%s"

let pattn typs   = List.mapi (fun i _ -> pvar (argn i)) typs
let pattl labels = List.map (fun { pld_name = { txt = n } } -> n, pvar (argl n)) labels

let pconstrrec name fields = pconstr name [precord ~closed:Closed fields]
let  constrrec name fields =  constr name [ record                fields]

let rec expr_of_typ ?decl typ =
  let loc = typ.ptyp_loc in
  let typ = Ppx_deriving.remove_pervasives ~deriver typ in
  match typ with
  | _ when Ppx_deriving.free_vars_in_core_type typ = [] -> [%expr fun x -> x]
  | { ptyp_desc = Ptyp_constr _ } ->
    let builtin = not (Attribute.has_flag ct_attr_nobuiltin typ) in
    begin match builtin, typ with
    | true, [%type: [%t? typ] list] ->
      [%expr Ppx_deriving_runtime.List.map [%e expr_of_typ ?decl typ]]
    | true, [%type: [%t? typ] array] ->
      [%expr Ppx_deriving_runtime.Array.map [%e expr_of_typ ?decl typ]]
    | true, [%type: [%t? typ] option] ->
      [%expr function None -> None | Some x -> Some ([%e expr_of_typ ?decl typ] x)]
    | true, ([%type: ([%t? ok_t], [%t? err_t]) result] |
             [%type: ([%t? ok_t], [%t? err_t]) Result.result]) ->
      [%expr
        function
        | Ok ok -> Ok ([%e expr_of_typ ?decl ok_t] ok)
        | Error err -> Error ([%e expr_of_typ ?decl err_t] err)]
    | _, { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
      app (Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix deriver) lid)))
          (List.map (expr_of_typ ?decl) args)
    | _ -> assert false
    end
  | { ptyp_desc = Ptyp_tuple typs } ->
    [%expr fun [%p ptuple (List.mapi (fun i _ -> pvar (argn i)) typs)] ->
      [%e tuple (List.mapi (fun i typ -> app (expr_of_typ ?decl typ) [evar (argn i)]) typs)]];
  | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
    let cases =
      fields |> List.map (fun field ->
        let pat_variant label popt =
          Pat.variant label.txt popt
        in
        let exp_variant label popt =
          Exp.variant label.txt popt
        in
        match field.prf_desc with
        | Rtag(label, true (*empty*), []) ->
          Exp.case (pat_variant label None) (exp_variant label None)
        | Rtag(label, false, [typ]) ->
          Exp.case (pat_variant label (Some [%pat? x]))
                   (exp_variant label (Some [%expr [%e expr_of_typ ?decl typ] x]))
        | Rinherit({ ptyp_desc = Ptyp_constr (tname, _) } as typ) -> begin
          match decl with
          | None ->
            raise_errorf "inheritance of polymorphic variants not supported"
          | Some(d) ->
            Exp.case [%pat? [%p Pat.type_ tname] as x]
                     [%expr ([%e expr_of_typ ?decl typ] x :> [%t Ppx_deriving.core_type_of_type_decl d])]
          end
        | _ ->
          raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                       deriver (Ppx_deriving.string_of_core_type typ))
    in
    Exp.function_ cases
  | { ptyp_desc = Ptyp_var name } -> evar ("poly_"^name)
  | { ptyp_desc = Ptyp_alias (typ, name) } ->
    [%expr fun x -> [%e evar ("poly_"^name.txt)] ([%e expr_of_typ ?decl typ] x)]
  | { ptyp_loc } ->
    raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                 deriver (Ppx_deriving.string_of_core_type typ)

and expr_of_label_decl ?decl { pld_type; pld_attributes } =
  let attrs = pld_type.ptyp_attributes @ pld_attributes in
  expr_of_typ ?decl { pld_type with ptyp_attributes = attrs }

let str_of_type ({ ptype_loc = loc } as type_decl) =
  let mapper =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest -> expr_of_typ ~decl:type_decl manifest
    | Ptype_variant constrs, _ ->
      constrs |>
      List.map (fun { pcd_name = { txt = name' }; pcd_args } ->
        match pcd_args with
        | Pcstr_tuple(typs) ->
          let args = List.mapi (fun i typ -> app (expr_of_typ ~decl:type_decl typ) [evar (argn i)]) typs in
          Exp.case (pconstr name' (pattn typs))
                   (constr name' args)
        | Pcstr_record(labels) ->
          let args = labels |> List.map (fun ({ pld_name = { txt = n }; _ } as pld) ->
                        n, [%expr [%e expr_of_label_decl ~decl:type_decl pld]
                               [%e evar (argl n)]]) in
          Exp.case (pconstrrec name' (pattl labels))
                   (constrrec name' args)
        ) |>
      Exp.function_
    | Ptype_record labels, _ ->
      let fields =
        labels |> List.mapi (fun i ({ pld_name = { txt = name }; _ } as pld) ->
          name, [%expr [%e expr_of_label_decl ~decl:type_decl pld]
                       [%e Exp.field (evar "x") (mknoloc (Lident name))]])
      in
      let annot_typ = Ppx_deriving.core_type_of_type_decl type_decl in
      [%expr fun (x:[%t annot_typ]) -> [%e record fields]]
    | Ptype_abstract, None ->
      raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    | Ptype_open, _        ->
      raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  [Vb.mk ~attrs:[Ppx_deriving.attr_warning [%expr "-39"]]
         (pvar (Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl))
         (polymorphize mapper)]

let sig_of_type type_decl =
  let loc = type_decl.ptype_loc in
  let typ_arg, var_arg, bound = Ppx_deriving.instantiate []    type_decl in
  let typ_ret, var_ret, _     = Ppx_deriving.instantiate bound type_decl in
  let arrow = Typ.arrow Label.nolabel in
  let poly_fns = List.map2 (fun a r -> [%type: [%t Typ.var a] -> [%t Typ.var r]])
                           var_arg var_ret in
  let typ = List.fold_right arrow poly_fns (arrow typ_arg typ_ret) in
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl)) typ)]

let impl_generator = Deriving.Generator.V2.make_noarg (fun ~ctxt:_ (_, type_decls) ->
  [Str.value Recursive (List.concat (List.map str_of_type type_decls))])

let intf_generator = Deriving.Generator.V2.make_noarg (fun ~ctxt:_ (_, type_decls) ->
  List.concat (List.map sig_of_type type_decls))

let deriving: Deriving.t =
  Deriving.add
    deriver
    ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
