open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

module StringSet = Ppx_deriving.StringSet

let deriver = "eq"
let raise_errorf = Ppx_deriving.raise_errorf

type eq_options =
  {
    allow_std_type_shadowing: bool;
  }

let default_eq_options =
  {
    allow_std_type_shadowing= false;
  }

let parse_options options =
  let option_parser acc (name, expr) =
    match name with
    | "allow_std_type_shadowing" -> { allow_std_type_shadowing = true }
    | _ ->
      raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name in
  List.fold_left option_parser default_eq_options options

let attr_equal attrs =
  Ppx_deriving.(attrs |> attr ~deriver "equal" |> Arg.(get_attr ~deriver expr))

let argn kind =
  Printf.sprintf (match kind with `lhs -> "lhs%d" | `rhs -> "rhs%d")

let pattn side typs =
  List.mapi (fun i _ -> pvar (argn side i)) typs

let core_type_of_decl ~options ~path type_decl =
  ignore (parse_options options);
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: [%t var] -> [%t var] -> bool])
    type_decl
    [%type: [%t typ] -> [%t typ] -> bool]

let sig_of_type ~options ~path type_decl =
  ignore (parse_options options);
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "equal") type_decl))
             (core_type_of_decl ~options ~path type_decl))]

let rec exprsn group_def typs =
  typs |> List.mapi (fun i typ ->
    app (expr_of_typ group_def typ) [evar (argn `lhs i); evar (argn `rhs i)])

and expr_of_typ group_def typ =
  match attr_equal typ.ptyp_attributes with
  | Some fn -> fn
  | None ->
    match typ with
    | { ptyp_desc = Ptyp_constr ({ txt = (Lident id as lid) }, args) } 
        when StringSet.mem id group_def ->
      let equal_fn = Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "equal") lid)) in
      app equal_fn (List.map (expr_of_typ group_def) args)
    | [%type: _] | [%type: unit] -> [%expr fun _ _ -> true]
    | [%type: int] | [%type: int32] | [%type: Int32.t]
    | [%type: int64] | [%type: Int64.t] | [%type: nativeint] | [%type: Nativeint.t]
    | [%type: float] | [%type: bool] | [%type: char] | [%type: string] |
      [%type: String.t] | [%type: bytes] ->
      [%expr (fun (a:[%t typ]) b -> a = b)]
    | [%type: [%t? typ] ref]   -> [%expr fun a b -> [%e expr_of_typ group_def typ] !a !b]
    | [%type: [%t? typ] list]  ->
      [%expr
        let rec loop x y =
          match x, y with
          | [], [] -> true
          | a :: x, b :: y -> [%e expr_of_typ group_def typ] a b && loop x y
          | _ -> false
        in (fun x y -> loop x y)]
    | [%type: [%t? typ] array] ->
      [%expr fun x y ->
        let rec loop i =
          (i = Array.length x || [%e expr_of_typ group_def typ] x.(i) y.(i)) && loop (i + 1)
        in Array.length x = Array.length y && loop 0]
    | [%type: [%t? typ] option] ->
      [%expr fun x y ->
        match x, y with
        | None, None -> true
        | Some a, Some b -> [%e expr_of_typ group_def typ] a b
        | _ -> false]
    | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
      let equal_fn = Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "equal") lid)) in
      app equal_fn (List.map (expr_of_typ group_def) args)
    | { ptyp_desc = Ptyp_tuple typs } ->
      [%expr fun [%p ptuple (pattn `lhs typs)] [%p ptuple (pattn `rhs typs)] ->
        [%e exprsn group_def typs |> Ppx_deriving.(fold_exprs (binop_reduce [%expr (&&)]))]]
    | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
      let cases =
        (fields |> List.map (fun field ->
          let pdup f = ptuple [f "lhs"; f "rhs"] in
          match field with
          | Rtag (label, _, true (*empty*), []) ->
            Exp.case (pdup (fun _ -> Pat.variant label None)) [%expr true]
          | Rtag (label, _, false, [typ]) ->
            Exp.case (pdup (fun var -> Pat.variant label (Some (pvar var))))
                     (app (expr_of_typ group_def typ) [evar "lhs"; evar "rhs"])
          | Rinherit ({ ptyp_desc = Ptyp_constr (tname, _) } as typ) ->
            Exp.case (pdup (fun var -> Pat.alias (Pat.type_ tname) (mknoloc var)))
                     (app (expr_of_typ group_def typ) [evar "lhs"; evar "rhs"])
          | _ ->
            raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                         deriver (Ppx_deriving.string_of_core_type typ))) @
        [Exp.case (pvar "_") [%expr false]]
      in
      [%expr fun lhs rhs -> [%e Exp.match_ [%expr lhs, rhs] cases]]
    | { ptyp_desc = Ptyp_var name } -> evar ("poly_"^name)
    | { ptyp_desc = Ptyp_alias (typ, _) } -> expr_of_typ group_def typ
    | { ptyp_loc } ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                   deriver (Ppx_deriving.string_of_core_type typ)

let str_of_type ~options ~path group_def ({ ptype_loc = loc } as type_decl) =
  ignore (parse_options options);
  let comparator =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest -> expr_of_typ group_def manifest
    | Ptype_variant constrs, _ ->
      let wildcard = match constrs with
        | [] | [_] -> []
        | _ :: _ :: _ -> [Exp.case (pvar "_") [%expr false]] in
      let cases =
        (constrs |> List.map (fun { pcd_name = { txt = name }; pcd_args = typs } ->
          exprsn group_def typs |>
          Ppx_deriving.(fold_exprs ~unit:[%expr true] (binop_reduce [%expr (&&)])) |>
          Exp.case (ptuple [pconstr name (pattn `lhs typs);
                            pconstr name (pattn `rhs typs)]))) @ wildcard 
      in
      [%expr fun lhs rhs -> [%e Exp.match_ [%expr lhs, rhs] cases]]
    | Ptype_record labels, _ ->
      let exprs =
        labels |> List.map (fun { pld_name = { txt = name }; pld_type; pld_attributes } ->
          (* combine attributes of type and label *)
          let attrs =  pld_type.ptyp_attributes @ pld_attributes in
          let pld_type = {pld_type with ptyp_attributes=attrs} in
          let field obj = Exp.field obj (mknoloc (Lident name)) in
          app (expr_of_typ group_def pld_type) [field (evar "lhs"); field (evar "rhs")])
      in
      [%expr fun lhs rhs -> [%e exprs |> Ppx_deriving.(fold_exprs (binop_reduce [%expr (&&)]))]]
    | Ptype_abstract, None ->
      raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    | Ptype_open, _ ->
      raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let out_type =
    Ppx_deriving.strong_type_of_type @@
      core_type_of_decl  ~options ~path type_decl in
  let eq_var = 
    pvar (Ppx_deriving.mangle_type_decl (`Prefix "equal") type_decl) in
  [Vb.mk (Pat.constraint_ eq_var out_type) (polymorphize comparator)]

let type_decl_str ~options ~path type_decls =
  let opts = parse_options options in
  let typename_set =
    Ppx_deriving.extract_typename_of_type_group 
      deriver
      ~allow_shadowing:opts.allow_std_type_shadowing
      type_decls in
  let here_loc = (List.hd type_decls).ptype_loc in
  if StringSet.mem "bool" typename_set then
    raise_errorf
      ~loc:here_loc
      "%s can't derivate types when shadowing bool (even with option)" deriver;
  let code =
    List.map (str_of_type ~options ~path typename_set) type_decls in
  [Str.value Recursive (List.concat code)]

let () =
  Ppx_deriving.(register (create deriver
    ~core_type:(expr_of_typ StringSet.empty)
    ~type_decl_str: type_decl_str
    ~type_decl_sig: (fun ~options ~path type_decls ->
       List.concat (List.map (sig_of_type ~options ~path) type_decls))
    ()
  ))
