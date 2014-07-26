open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let raise_errorf = Ppx_deriving.raise_errorf

let mappings_of_type_decl ~prefix ~name type_decl =
  let map acc mappings attrs constr_name =
    let value =
      match Ppx_deriving.attr ~prefix "value" attrs |>
            Ppx_deriving.Arg.(payload ~name int) with
      | Some idx -> idx | None -> acc
    in
    (value + 1, (value, constr_name) :: mappings)
  in
  let kind, (_, mappings) =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_variant constrs, _ ->
      `Regular,
      List.fold_left (fun (acc, mappings) { pcd_name; pcd_args; pcd_attributes; pcd_loc } ->
          if pcd_args <> [] then
            raise_errorf ~loc:pcd_loc "%s can be derived only for argumentless constructors" name;
          map acc mappings pcd_attributes pcd_name)
        (0, []) constrs
    | Ptype_abstract, Some { ptyp_desc = Ptyp_variant (constrs, Closed, None); ptyp_loc } ->
      `Polymorphic,
      List.fold_left (fun (acc, mappings) row_field ->
          (* TODO: use row_field location instead of ptyp_loc when fixed in Parsetree *)
          match row_field with
          | Rinherit _ ->
            raise_errorf ~loc:ptyp_loc "%s cannot be derived for inherited variant cases" name
          | Rtag (name, attrs, true, []) ->
            map acc mappings attrs { txt = name; loc = ptyp_loc }
          | Rtag _ ->
            raise_errorf ~loc:ptyp_loc "%s can be derived only for argumentless constructors" name)
        (0, []) constrs
    | _ -> raise_errorf ~loc:type_decl.ptype_loc "%s can be derived only for variants" name
  in
  let rec check_dup mappings =
    match mappings with
    | (a, { txt=atxt; loc=aloc }) :: (b, { txt=btxt; loc=bloc }) :: _ when a = b ->
      let sigil = match kind with `Regular -> "" | `Polymorphic -> "`" in
      let sub = [Location.errorf ~loc:bloc "Same as for %s%s" sigil btxt] in
      raise_errorf ~sub ~loc:aloc "%s: duplicate value %d for constructor %s%s" name a sigil atxt
    | _ :: rest -> check_dup rest
    | [] -> ()
  in
  mappings |> List.stable_sort (fun (a,_) (b,_) -> compare a b) |> check_dup;
  kind, mappings
