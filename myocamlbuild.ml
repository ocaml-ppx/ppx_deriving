open Ocamlbuild_plugin

let () = dispatch (
  function
  | After_rules ->

    ocaml_lib ~dir:"src/" "ppx_deriving";

    let flags_for name =
      flag ["ocaml"; "compile"; "use_deriving_" ^ name] &
        S[A"-ppx"; A("src/ppx_deriving_main.native src_plugins/ppx_deriving_"^name^".cmxs")]
    in
    List.iter flags_for ["show"; "eq"; "compare"]

  | _ -> ())
