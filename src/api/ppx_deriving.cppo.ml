open Ppxlib

open Asttypes
open Ast_helper

module Ast_convenience = struct
  (* Formerly defined in Ppx_tools.Ast_convenience.
     Ppx_tools is not compatible with Ppxlib. *)

  let mkloc txt loc =
    { txt; loc }

  let mknoloc txt =
    mkloc txt !Ast_helper.default_loc

  let str_of_string s =
    mknoloc s

  let lid_of_string s =
    mknoloc (Longident.parse s)

  let unit () =
    let loc = !Ast_helper.default_loc in
    [%expr ()]

  let punit () =
    let loc = !Ast_helper.default_loc in
    [%pat? ()]

  let str s =
    Ast_helper.Exp.constant (Ast_helper.Const.string s)

  let int i =
    Ast_helper.Exp.constant (Ast_helper.Const.int i)

  let pint i =
    Ast_helper.Pat.constant (Ast_helper.Const.int i)

  let evar name =
    Ast_helper.Exp.ident (lid_of_string name)

  let pvar name =
    Ast_helper.Pat.var (str_of_string name)

  let app f args =
    match args with
    | [] -> f
    | _ ->
        let args = List.map (fun e -> (Nolabel, e)) args in
        Ast_helper.Exp.apply f args

  let constr name args =
    let args =
      match args with
      | [] -> None
      | [arg] -> Some arg
      | _ -> Some (Ast_helper.Exp.tuple args) in
    Ast_helper.Exp.construct (lid_of_string name) args

  let pconstr name args =
    let args =
      match args with
      | [] -> None
      | [arg] -> Some arg
      | _ -> Some (Ast_helper.Pat.tuple args) in
    Ast_helper.Pat.construct (lid_of_string name) args

  let tconstr name args =
    Ast_helper.Typ.constr (lid_of_string name) args

  let record fields =
    let fields =
      List.map (fun (name, value) -> (lid_of_string name, value)) fields in
    Ast_helper.Exp.record fields None

  let precord ~closed fields =
    let fields =
      List.map (fun (name, value) -> (lid_of_string name, value)) fields in
    Ast_helper.Pat.record fields closed

  let tuple items =
    match items with
    | [] -> unit ()
    | [item] -> item
    | _ -> Ast_helper.Exp.tuple items

  let ptuple items =
    match items with
    | [] -> punit ()
    | [item] -> item
    | _ -> Ast_helper.Pat.tuple items

  let attribute_has_name name attribute =
    attribute.attr_name.txt = name

  let has_attr name attributes =
    List.exists (attribute_has_name name) attributes

  let find_attr name attributes =
    match List.find (attribute_has_name name) attributes with
    | exception Not_found -> None
    | attribute -> Some attribute.attr_payload

  module Label = struct
    let nolabel = Nolabel

    let labelled s =
      Labelled s

    let optional s =
      Optional s
  end
end

open Ast_convenience

type tyvar = string Location.loc

type deriver = {
  name : string ;
  core_type : (core_type -> expression) option;
  type_decl_str : options:(string * expression) list -> path:string list ->
                   type_declaration list -> structure;
  type_ext_str : options:(string * expression) list -> path:string list ->
                  type_extension -> structure;
  module_type_decl_str : options:(string * expression) list ->
                          path:string list ->
                          module_type_declaration -> structure;
  type_decl_sig : options:(string * expression) list -> path:string list ->
                   type_declaration list -> signature;
  type_ext_sig : options:(string * expression) list -> path:string list ->
                  type_extension -> signature;
  module_type_decl_sig : options:(string * expression) list ->
                          path:string list ->
                          module_type_declaration -> signature;
}

type Ppx_derivers.deriver += T of deriver

type internal_or_external =
  | Internal of deriver
  | External of string

let hooks = Queue.create ()

let add_register_hook f = Queue.add f hooks

let register d =
  Ppx_derivers.register d.name (T d);
  Queue.iter (fun f -> f d) hooks

let derivers () =
  List.fold_left
    (fun acc (_name, drv) ->
       match drv with
       | T d -> d :: acc
       | _ -> acc)
    [] (Ppx_derivers.derivers ())

let lookup_internal_or_external name =
  match Ppx_derivers.lookup name with
  | Some (T d) -> Some (Internal d)
  | Some _ -> Some (External name)
  | None -> None

let lookup name =
  match lookup_internal_or_external name with
  | Some (Internal d) -> Some d
  | Some (External _) | None -> None

let raise_errorf ?sub ?loc fmt =
  let module Location = Ocaml_common.Location in
  let raise_msg str =
#if OCAML_VERSION >= (4, 08, 0)
    let sub =
      let msg_of_error err =
        { txt = (fun fmt -> Location.print_report fmt err);
          loc = err.Location.main.loc } in
      Option.map (List.map msg_of_error) sub in
#endif
    let err = Location.error ?sub ?loc str in
    raise (Location.Error err) in
  Printf.kprintf raise_msg fmt

let create =
  let def_ext_str name ~options ~path typ_ext =
    raise_errorf "Extensible types in structures not supported by deriver %s" name
  in
  let def_ext_sig name ~options ~path typ_ext =
    raise_errorf "Extensible types in signatures not supported by deriver %s" name
  in
  let def_decl_str name ~options ~path typ_decl =
    raise_errorf "Type declarations in structures not supported by deriver %s" name
  in
  let def_decl_sig name ~options ~path typ_decl =
    raise_errorf "Type declarations in signatures not supported by deriver %s" name
  in
  let def_module_type_decl_str name ~options ~path module_type_decl =
    raise_errorf "Module type declarations in structures not supported by \
                  deriver %s" name
  in
  let def_module_type_decl_sig name ~options ~path module_type_decl =
    raise_errorf "Module type declarations in signatures not supported by \
                  deriver %s" name
  in
  fun name ?core_type
    ?(type_ext_str=def_ext_str name)
    ?(type_ext_sig=def_ext_sig name)
    ?(type_decl_str=def_decl_str name)
    ?(type_decl_sig=def_decl_sig name)
    ?(module_type_decl_str=def_module_type_decl_str name)
    ?(module_type_decl_sig=def_module_type_decl_sig name)
    () ->
      { name ; core_type ;
        type_decl_str ; type_ext_str ; module_type_decl_str ;
        type_decl_sig ; type_ext_sig ; module_type_decl_sig ;
      }

let string_of_core_type typ =
  Format.asprintf "%a" Pprintast.core_type { typ with ptyp_attributes = [] }

let string_of_constant_opt (constant : Parsetree.constant) : string option =
  match constant with
  | Pconst_string (s, _, _) ->
      Some s
  | _ -> None

let string_of_expression_opt (e : Parsetree.expression) : string option =
  match e with
  | { pexp_desc = Pexp_constant constant } ->
      string_of_constant_opt constant
  | _ -> None

module Arg = struct
  type 'a conv = expression -> ('a, string) Result.result

  open Result
  let expr expr = Ok expr

  let int expr =
    match expr with
    | { pexp_desc = Pexp_constant (Pconst_integer (sn, _)) } -> Ok (int_of_string sn)
    | _ -> Error "integer"

  let bool expr =
    match expr with
    | [%expr true] -> Ok true
    | [%expr false] -> Ok false
    | _ -> Error "boolean"

  let string expr =
    match expr with
    | { pexp_desc = Pexp_constant (Pconst_string (n, _, None)) } -> Ok n
    | _ -> Error "string"

  let char = function
    | { pexp_desc = Pexp_constant (Pconst_char c) } -> Ok c
    | _ -> Error "char"

  let enum values expr =
    match expr with
    | { pexp_desc = Pexp_variant (name, None) }
      when List.mem name values -> Ok name
    | _ -> Error (Printf.sprintf "one of: %s"
                    (String.concat ", " (List.map (fun s -> "`"^s) values)))

  let list expr =
    let rec loop acc = function
      | [%expr []] -> Ok (List.rev acc)
      | [%expr [%e? x]::[%e? xs]] ->
        begin match expr x with
        | Ok v -> loop (v::acc) xs
        | Error e -> Error ("list:" ^ e)
        end
      | _ -> Error "list"
    in loop []

  let get_attr ~deriver conv attr =
    match attr with
    | None -> None
    | Some { attr_name = {txt = name; loc = _};
      attr_payload = PStr [{ pstr_desc = Pstr_eval (expr, []) }]; attr_loc = _ } ->
      begin match conv expr with
      | Ok v -> Some v
      | Error desc ->
        raise_errorf ~loc:expr.pexp_loc "%s: invalid [@%s]: %s expected" deriver name desc
      end
    | Some { attr_name = {txt = name; loc}; attr_payload = _; attr_loc = _ } ->
      raise_errorf ~loc "%s: invalid [@%s]: value expected" deriver name

  let get_flag ~deriver attr =
    match attr with
    | None -> false
    | Some { attr_name = _; attr_payload = PStr []; attr_loc = _ } -> true
    | Some { attr_name = {txt = name; loc}; attr_payload = _; attr_loc = _ } ->
      raise_errorf ~loc "%s: invalid [@%s]: empty structure expected" deriver name

  let get_expr ~deriver conv expr =
    match conv expr with
    | Error desc -> raise_errorf ~loc:expr.pexp_loc "%s: %s expected" deriver desc
    | Ok v -> v
end

let attr_warning expr =
  let loc = !default_loc in
  let structure = {pstr_desc = Pstr_eval (expr, []); pstr_loc = loc} in
  { attr_name = { txt = "ocaml.warning"; loc; };
    attr_payload = PStr [structure];
    attr_loc = loc;
  }

type quoter = {
  mutable next_id : int;
  mutable bindings : value_binding list;
}

let create_quoter () = { next_id = 0; bindings = [] }

let quote ~quoter expr =
  let loc = !Ast_helper.default_loc in
  let name = "__" ^ string_of_int quoter.next_id in
  quoter.bindings <- (Vb.mk (pvar name) [%expr fun () -> [%e expr]]) :: quoter.bindings;
  quoter.next_id <- quoter.next_id + 1;
  [%expr [%e evar name] ()]

let sanitize ?(module_=Lident "Ppx_deriving_runtime") ?(quoter=create_quoter ()) expr =
  let body =
    let loc = !Ast_helper.default_loc in
    let attrs = [attr_warning [%expr "-A"]] in
    let modname = { txt = module_; loc } in
    Exp.open_ ~loc ~attrs
      (Opn.mk ~loc ~attrs ~override:Override (Mod.ident ~loc ~attrs modname))
      expr in
  match quoter.bindings with
  | [] -> body
  | bindings -> Exp.let_ Nonrecursive bindings body

let with_quoter fn a =
  let quoter = create_quoter () in
  sanitize ~quoter (fn quoter a)

let expand_path ~path ident =
  String.concat "." (path @ [ident])

let path_of_type_decl ~path type_decl =
  match type_decl.ptype_manifest with
  | Some { ptyp_desc = Ptyp_constr ({ txt = lid }, _) } ->
    begin match lid with
    | Lident _ -> []
    | Ldot (lid, _) -> Ocaml_common.Longident.flatten lid
    | Lapply _ -> assert false
    end
  | _ -> path

let mangle ?(fixpoint="t") affix name =
  match name = fixpoint, affix with
  | true,  (`Prefix x | `Suffix x) -> x
  | true, `PrefixSuffix (p, s) -> p ^ "_" ^ s
  | false, `PrefixSuffix (p, s) -> p ^ "_" ^ name ^ "_" ^ s
  | false, `Prefix x -> x ^ "_" ^ name
  | false, `Suffix x -> name ^ "_" ^ x

let mangle_type_decl ?fixpoint affix { ptype_name = { txt = name } } =
  mangle ?fixpoint affix name

let mangle_lid ?fixpoint affix lid =
  match lid with
  | Lident s    -> Lident (mangle ?fixpoint affix s)
  | Ldot (p, s) -> Ldot (p, mangle ?fixpoint affix s)
  | Lapply _    -> assert false

let attr ~deriver name attrs =
  let starts prefix str =
    String.length str >= String.length prefix &&
      String.sub str 0 (String.length prefix) = prefix
  in
  let attr_starts prefix attr = starts prefix attr.attr_name.txt in
  let attr_is name attr = name = attr.attr_name.txt in
  let try_prefix prefix f =
    if List.exists (attr_starts prefix) attrs
    then prefix ^ name
    else f ()
  in
  let name =
    try_prefix ("deriving."^deriver^".") (fun () ->
      try_prefix (deriver^".") (fun () ->
        name))
  in
  try Some (List.find (attr_is name) attrs)
  with Not_found -> None

let attr_nobuiltin ~deriver attrs =
  attrs |> attr ~deriver "nobuiltin" |> Arg.get_flag ~deriver

let rec remove_pervasive_lid = function
  | Lident _ as lid -> lid
  | Ldot (Lident "Pervasives", s) -> Lident s
  | Ldot (Lident "Stdlib", s) -> Lident s
  | Ldot (lid, s) -> Ldot (remove_pervasive_lid lid, s)
  | Lapply (lid, lid2) ->
    Lapply (remove_pervasive_lid lid, remove_pervasive_lid lid2)

let remove_pervasives ~deriver typ =
  if attr_nobuiltin ~deriver typ.ptyp_attributes then typ
  else
    let mapper = object
        inherit Ppxlib.Ast_traverse.map as super

        method! core_type typ =
          match super#core_type typ with
          | { ptyp_desc = Ptyp_constr (lid, l)} ->
              let lid = {lid with txt = remove_pervasive_lid lid.txt} in
              {typ with ptyp_desc = Ptyp_constr (lid, l)}
          | { ptyp_desc = Ptyp_class (lid, l)} ->
              let lid = {lid with txt = remove_pervasive_lid lid.txt} in
              {typ with ptyp_desc = Ptyp_class (lid, l)}
          | typ -> typ
    end in
    mapper#core_type typ

let mkloc = Ocaml_common.Location.mkloc

let fold_left_type_params fn accum params =
  List.fold_left (fun accum (param, _) ->
      match param with
      | { ptyp_desc = Ptyp_any } -> accum
      | { ptyp_desc = Ptyp_var name } ->
        let name = mkloc name param.ptyp_loc in
        fn accum name
      | _ -> assert false)
    accum params

let fold_left_type_decl fn accum { ptype_params } =
  fold_left_type_params fn accum ptype_params

let fold_left_type_ext fn accum { ptyext_params } =
  fold_left_type_params fn accum ptyext_params

let fold_right_type_params fn params accum =
  List.fold_right (fun (param, _) accum ->
      match param with
      | { ptyp_desc = Ptyp_any } -> accum
      | { ptyp_desc = Ptyp_var name } ->
        let name = mkloc name param.ptyp_loc in
        fn name accum
      | _ -> assert false)
    params accum

let fold_right_type_decl fn { ptype_params } accum =
  fold_right_type_params fn ptype_params accum

let fold_right_type_ext fn { ptyext_params } accum =
  fold_right_type_params fn ptyext_params accum

let free_vars_in_core_type typ =
  let rec free_in typ =
    match typ with
    | { ptyp_desc = Ptyp_any } -> []
    | { ptyp_desc = Ptyp_var name } ->
      [mkloc name typ.ptyp_loc]
    | { ptyp_desc = Ptyp_arrow (_, x, y) } -> free_in x @ free_in y
    | { ptyp_desc = (Ptyp_tuple xs | Ptyp_constr (_, xs)) } ->
      List.map free_in xs |> List.concat
    | { ptyp_desc = Ptyp_alias (x, name) } ->
      [mkloc name typ.ptyp_loc]
      @ free_in x
    | { ptyp_desc = Ptyp_poly (bound, x) } ->
      List.filter (fun y -> not (List.mem y bound)) (free_in x)
    | { ptyp_desc = Ptyp_variant (rows, _, _) } ->
      List.map (
          function { prf_desc = Rtag(_,_,ts) } -> List.map free_in ts
                 | { prf_desc = Rinherit(t) } -> [free_in t]
        ) rows |> List.concat |> List.concat
    | _ -> assert false
  in
  let uniq lst =
    let module StringSet = Set.Make(String) in
    let add (rev_names, txts) name =
      let txt =
        name.txt
      in
      if StringSet.mem txt txts
      then (rev_names, txts)
      else (name :: rev_names, StringSet.add txt txts)
    in List.rev (fst (List.fold_left add ([], StringSet.empty) lst))
  in free_in typ |> uniq

let var_name_of_int i =
  let letter = "abcdefghijklmnopqrstuvwxyz" in
  let rec loop i =
    if i < 26 then [letter.[i]] else letter.[i mod 26] :: loop (i / 26)
  in
  String.concat "" (List.map (String.make 1) (loop i))

let fresh_var bound =
  let rec loop i =
    let var_name = var_name_of_int i in
    if List.mem var_name bound then loop (i + 1) else var_name
  in
  loop 0

let poly_fun_of_type_decl type_decl expr =
  fold_right_type_decl (fun name expr ->
    let name = name.txt in
    Exp.fun_ Label.nolabel None (pvar ("poly_"^name)) expr) type_decl expr

let poly_fun_of_type_ext type_ext expr =
  fold_right_type_ext (fun name expr ->
    let name = name.txt in
    Exp.fun_ Label.nolabel None (pvar ("poly_"^name)) expr) type_ext expr

let poly_apply_of_type_decl type_decl expr =
  fold_left_type_decl (fun expr name ->
    let name = name.txt in
    Exp.apply expr [Label.nolabel, evar ("poly_"^name)]) expr type_decl

let poly_apply_of_type_ext type_ext expr =
  fold_left_type_ext (fun expr name ->
    let name = name.txt in
    Exp.apply expr [Label.nolabel, evar ("poly_"^name)]) expr type_ext

let poly_arrow_of_type_decl fn type_decl typ =
  fold_right_type_decl (fun name typ ->
    let name = name.txt in
    Typ.arrow Label.nolabel (fn (Typ.var name)) typ) type_decl typ

let poly_arrow_of_type_ext fn type_ext typ =
  fold_right_type_ext (fun name typ ->
    let var =
      Typ.var ~loc:name.loc name.txt
    in
    Typ.arrow Label.nolabel (fn var) typ) type_ext typ

let core_type_of_type_decl { ptype_name = name; ptype_params } =
  let name = mkloc (Lident name.txt) name.loc in
  Typ.constr name (List.map fst ptype_params)

let core_type_of_type_ext { ptyext_path ; ptyext_params } =
  Typ.constr ptyext_path (List.map fst ptyext_params)

let instantiate bound type_decl =
  let vars, bound =
    List.fold_right
      (fun _ (vars, bound) ->
        let v = fresh_var bound in (v :: vars, v :: bound))
      (free_vars_in_core_type (core_type_of_type_decl type_decl))
      ([], bound)
  in
  let vars = List.rev vars in
  let core_type = core_type_of_type_decl
    { type_decl with
        ptype_params = List.map2 (fun v (_, variance) -> Typ.var v, variance)
                                 vars type_decl.ptype_params }
  in
  core_type, vars, bound

let fold_exprs ?unit fn exprs =
  match exprs with
  | [a] -> a
  | hd::tl -> List.fold_left fn hd tl
  | [] ->
    match unit with
    | Some x -> x
    | None -> raise (Invalid_argument "Ppx_deriving.fold_exprs")

let seq_reduce ?sep a b =
  let loc = !Ast_helper.default_loc in
  match sep with
  | Some x -> [%expr [%e a]; [%e x]; [%e b]]
  | None -> [%expr [%e a]; [%e b]]

let binop_reduce x a b =
  let loc = !Ast_helper.default_loc in
  [%expr [%e x] [%e a] [%e b]]

let strong_type_of_type ty =
  let free_vars = free_vars_in_core_type ty in
  Typ.force_poly @@ Typ.poly free_vars ty

type deriver_options =
  | Options of (string * expression) list
  | Unknown_syntax

let derive path pstr_loc item attributes fn arg =
  let deriving = find_attr "deriving" attributes in
  let deriver_exprs, loc =
    match deriving with
    | Some (PStr [{ pstr_desc = Pstr_eval (
                    { pexp_desc = Pexp_tuple exprs }, []); pstr_loc }]) ->
      exprs, pstr_loc
    | Some (PStr [{ pstr_desc = Pstr_eval (
                    { pexp_desc = (Pexp_ident _ | Pexp_apply _) } as expr, []); pstr_loc }]) ->
      [expr], pstr_loc
    | _ -> raise_errorf ~loc:pstr_loc "Unrecognized [@@deriving] annotation syntax"
  in
  List.fold_left (fun items deriver_expr ->
      let name, options =
        match deriver_expr with
        | { pexp_desc = Pexp_ident name } ->
          name, Options []
        | { pexp_desc = Pexp_apply ({ pexp_desc = Pexp_ident name }, [label,
            { pexp_desc = Pexp_record (options, None) }]) }
              when label = Label.nolabel ->
          name,
          Options
            (options |> List.map (fun ({ txt }, expr) ->
               String.concat "." (Ocaml_common.Longident.flatten txt), expr))
        | { pexp_desc = Pexp_apply ({ pexp_desc = Pexp_ident name }, _) } ->
          name, Unknown_syntax
        | { pexp_loc } ->
          raise_errorf ~loc:pexp_loc "Unrecognized [@@deriving] syntax"
      in
      let name, loc = String.concat "_" (Ocaml_common.Longident.flatten name.txt), name.loc in
      let is_optional, options =
        match options with
        | Unknown_syntax -> false, options
        | Options options' ->
          match List.assoc "optional" options' with
          | exception Not_found -> false, options
          | expr ->
            Arg.(get_expr ~deriver:name bool) expr,
            Options (List.remove_assoc "optional" options')
      in
      match lookup_internal_or_external name, options with
      | Some (Internal deriver), Options options ->
        items @ ((fn deriver) ~options ~path:(!path) arg)
      | Some (Internal _), Unknown_syntax ->
        raise_errorf ~loc:deriver_expr.pexp_loc "Unrecognized [@@deriving] option syntax"
      | Some (External _), _ -> items
      | None, _ ->
        if is_optional then items
        else raise_errorf ~loc "Cannot locate deriver %s" name)
    [item] deriver_exprs

let derive_type_decl path typ_decls pstr_loc item fn =
  let attributes = List.concat (List.map (fun { ptype_attributes = attrs } -> attrs) typ_decls) in
  derive path pstr_loc item attributes fn typ_decls

let derive_type_ext path typ_ext pstr_loc item fn =
  let attributes = typ_ext.ptyext_attributes in
  derive path pstr_loc item attributes fn typ_ext

let derive_module_type_decl path module_type_decl pstr_loc item fn =
  let attributes = module_type_decl.pmtd_attributes in
  derive path pstr_loc item attributes fn module_type_decl

let module_from_input_name () =
  match !Ocaml_common.Location.input_name with
  | ""
  | "//toplevel//" -> []
  | filename ->
    let capitalize =
      String.capitalize_ascii
    in
    match Filename.chop_suffix filename ".ml" with
      | exception _ ->
         (* see https://github.com/ocaml-ppx/ppx_deriving/pull/196 *)
         []
      | path ->
         [capitalize (Filename.basename path)]

let pstr_desc_rec_flag pstr =
  match pstr with
  | Pstr_type(rec_flag, typ_decls) ->
    rec_flag
  | _ -> assert false

let module_nesting = ref []

let with_module name f =
  let old_nesting = !module_nesting in
  begin match name with
  | Some name -> module_nesting := !module_nesting @ [name]
  | None -> ()
  end;
  let result = f () in
  module_nesting := old_nesting;
  result

class mapper = object (self)
  inherit Ast_traverse.map as super

  method! expression expr =
    match expr with
    | { pexp_desc = Pexp_extension ({ txt = name; loc }, payload) }
        when String.(length name >= 7 && sub name 0 7 = "derive.") ->
      let name = String.sub name 7 ((String.length name) - 7) in
      let deriver =
        match lookup_internal_or_external name with
        | Some (Internal { core_type = Some deriver }) -> deriver
        | Some _ ->
            raise_errorf ~loc "Deriver %s does not support inline notation"
              name
        | None -> raise_errorf ~loc "Cannot locate deriver %s" name
      in
      begin match payload with
      | PTyp typ -> deriver typ
      | _ -> raise_errorf ~loc "Unrecognized [%%derive.*] syntax"
      end
    | { pexp_desc = Pexp_extension ({ txt = name; loc }, PTyp typ) } ->
      begin match lookup_internal_or_external name with
      | Some (Internal { core_type = Some deriver }) ->
        Ast_helper.with_default_loc typ.ptyp_loc (fun () ->
          deriver typ)
      | _ -> super#expression expr
      end
    | _ -> super#expression expr

  method! structure items =
    match items with
    | { pstr_desc = Pstr_type(_, typ_decls) as pstr_desc ; pstr_loc } :: rest when
        List.exists (fun ty -> has_attr "deriving" ty.ptype_attributes) typ_decls
        && pstr_desc_rec_flag pstr_desc = Nonrecursive ->
      raise_errorf ~loc:pstr_loc "The nonrec flag is not supported by ppx_deriving"
    | { pstr_desc = Pstr_type(_, typ_decls); pstr_loc } as item :: rest when
        List.exists (fun ty -> has_attr "deriving" ty.ptype_attributes) typ_decls ->
      let derived =
        Ast_helper.with_default_loc pstr_loc (fun () ->
          derive_type_decl module_nesting typ_decls pstr_loc item
            (fun deriver -> deriver.type_decl_str))
      in derived @ self#structure rest
    | { pstr_desc = Pstr_typext typ_ext; pstr_loc } as item :: rest when
          has_attr "deriving" typ_ext.ptyext_attributes ->
      let derived =
        Ast_helper.with_default_loc pstr_loc (fun () ->
          derive_type_ext module_nesting typ_ext pstr_loc item
            (fun deriver -> deriver.type_ext_str))
      in derived @ self#structure rest
    | { pstr_desc = Pstr_modtype modtype; pstr_loc } as item :: rest when
          has_attr "deriving" modtype.pmtd_attributes ->
      let derived =
        Ast_helper.with_default_loc pstr_loc (fun () ->
          derive_module_type_decl module_nesting modtype pstr_loc
            item (fun deriver -> deriver.module_type_decl_str))
      in derived @ self#structure rest
    | { pstr_desc = Pstr_module ({ pmb_name = { txt = name } } as mb) } as item :: rest ->
      let derived =
        { item with pstr_desc = Pstr_module (
            with_module name
              (fun () -> self#module_binding mb)) }
      in derived :: self#structure rest
    | { pstr_desc = Pstr_recmodule mbs } as item :: rest ->
      let derived =
        { item with pstr_desc = Pstr_recmodule (
            mbs |> List.map (fun ({ pmb_name = { txt = name } } as mb) ->
              with_module name
                (fun () -> self#module_binding mb))) }
      in derived :: self#structure rest
    | { pstr_loc } as item :: rest ->
      let derived = self#structure_item item
      in derived :: self#structure rest
    | [] -> []

  method! signature items =
    match items with
    | { psig_desc = Psig_type(_, typ_decls); psig_loc } as item :: rest when
        List.exists (fun ty -> has_attr "deriving" ty.ptype_attributes)
          typ_decls ->
      let derived =
        Ast_helper.with_default_loc psig_loc (fun () ->
          derive_type_decl module_nesting typ_decls psig_loc item
            (fun deriver -> deriver.type_decl_sig))
      in derived @ self#signature rest
    | { psig_desc = Psig_typext typ_ext; psig_loc } as item :: rest when
        has_attr "deriving" typ_ext.ptyext_attributes ->
      let derived =
        Ast_helper.with_default_loc psig_loc (fun () ->
          derive_type_ext module_nesting typ_ext psig_loc item
               (fun deriver -> deriver.type_ext_sig))
      in derived @ self#signature rest
    | { psig_desc = Psig_modtype modtype; psig_loc } as item :: rest when
        has_attr "deriving" modtype.pmtd_attributes ->
      let derived =
        Ast_helper.with_default_loc psig_loc (fun () ->
          derive_module_type_decl module_nesting modtype psig_loc item
            (fun deriver -> deriver.module_type_decl_sig))
      in derived @ self#signature rest
    | { psig_desc = Psig_module ({ pmd_name = { txt = name } } as md) } as item :: rest ->
      let derived =
        { item with psig_desc = Psig_module (
            with_module name
              (fun () -> self#module_declaration md)) }
      in derived :: self#signature rest
    | { psig_desc = Psig_recmodule mds } as item :: rest ->
      let derived =
        { item with psig_desc = Psig_recmodule (
            mds |> List.map (fun ({ pmd_name = { txt = name } } as md) ->
              with_module name
                (fun () -> self#module_declaration md))) }
      in derived :: self#signature rest
    | { psig_loc } as item :: rest ->
      let derived = self#signature_item item
      in derived :: self#signature rest
    | [] -> []
end

let map_structure s =
  module_nesting := module_from_input_name ();
  (new mapper)#structure s

let map_signature s =
  module_nesting := module_from_input_name ();
  (new mapper)#signature s

let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

(* This is only used when ppx_deriving is linked as part of an ocaml-migrate-parsetre
   driver. *)
let () =
  Ppxlib.Driver.register_transformation "ppx_deriving"
    ~impl:map_structure
    ~intf:map_signature
