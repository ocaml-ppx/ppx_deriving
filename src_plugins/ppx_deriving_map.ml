open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let deriver = "map"
let raise_errorf = Ppx_deriving.raise_errorf

let parse_options options =
  options |> List.iter (fun (name, expr) ->
    match name with
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name)

let attr_nobuiltin attrs =
  Ppx_deriving.(attrs |> attr ~deriver "nobuiltin" |> Arg.get_flag ~deriver)

let argn = Printf.sprintf "a%d"
let argl = Printf.sprintf "a%s"

let pattn typs   = List.mapi (fun i _ -> pvar (argn i)) typs
let pattl labels = List.map (fun { pld_name = { txt = n } } -> n, pvar (argl n)) labels

let pconstrrec name fields = pconstr name [precord ~closed:Closed fields]
let  constrrec name fields =  constr name [ record                fields]

let rec expr_of_typ ?decl typ =
  let typ = Ppx_deriving.remove_pervasives ~deriver typ in
  match typ with
  | _ when Ppx_deriving.free_vars_in_core_type typ = [] -> [%expr fun x -> x]
  | { ptyp_desc = Ptyp_constr _ } ->
    let builtin = not (attr_nobuiltin typ.ptyp_attributes) in
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
        | Result.Ok ok -> Result.Ok ([%e expr_of_typ ?decl ok_t] ok)
        | Result.Error err -> Result.Error ([%e expr_of_typ ?decl err_t] err)]
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
        match field with
        | Rtag (label, _, true (*empty*), []) ->
          Exp.case (Pat.variant label None) (Exp.variant label None)
        | Rtag (label, _, false, [typ]) ->
          Exp.case (Pat.variant label (Some [%pat? x]))
                   (Exp.variant label (Some [%expr [%e expr_of_typ ?decl typ] x]))
        | Rinherit ({ ptyp_desc = Ptyp_constr (tname, _) } as typ) -> begin
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
    [%expr fun x -> [%e evar ("poly_"^name)] ([%e expr_of_typ ?decl typ] x)]
  | { ptyp_loc } ->
    raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                 deriver (Ppx_deriving.string_of_core_type typ)

let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  parse_options options;
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
          let args = labels |> List.map (fun { pld_name = { txt = n }; pld_type = typ } ->
                        n, [%expr [%e expr_of_typ ~decl:type_decl typ] [%e evar (argl n)]]) in
          Exp.case (pconstrrec name' (pattl labels))
                   (constrrec name' args)
        ) |>
      Exp.function_
    | Ptype_record labels, _ ->
      let fields =
        labels |> List.mapi (fun i { pld_name = { txt = name }; pld_type } ->
          name, [%expr [%e expr_of_typ ~decl:type_decl pld_type]
                       [%e Exp.field (evar "x") (mknoloc (Lident name))]])
      in
      [%expr fun x -> [%e record fields]]
    | Ptype_abstract, None ->
      raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    | Ptype_open, _        ->
      raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  [Vb.mk ~attrs:[Ppx_deriving.attr_warning [%expr "-39"]]
         (pvar (Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl))
         (polymorphize mapper)]

let sig_of_type ~options ~path type_decl =
  parse_options options;
  let typ_arg, var_arg, bound = Ppx_deriving.instantiate []    type_decl in
  let typ_ret, var_ret, _     = Ppx_deriving.instantiate bound type_decl in
  let arrow = Typ.arrow Label.nolabel in
  let poly_fns = List.map2 (fun a r -> [%type: [%t Typ.var a] -> [%t Typ.var r]])
                           var_arg var_ret in
  let typ = List.fold_right arrow poly_fns (arrow typ_arg typ_ret) in
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl)) typ)]

let () =
  Ppx_deriving.(register (create deriver
    ~core_type: (expr_of_typ ?decl:None)
    ~type_decl_str: (fun ~options ~path type_decls ->
      [Str.value Recursive (List.concat (List.map (str_of_type ~options ~path) type_decls))])
    ~type_decl_sig: (fun ~options ~path type_decls ->
      List.concat (List.map (sig_of_type ~options ~path) type_decls))
    ()
  ))
