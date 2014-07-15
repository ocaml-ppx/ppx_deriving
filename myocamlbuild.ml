(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin;;

dispatch
  (MyOCamlbuildBase.dispatch_combine [
    begin function
    | After_rules ->
      let flags_for name =
        flag ["ocaml"; "compile"; "use_deriving_" ^ name] &
          S[A"-ppx"; A("lib/ppx_deriving_main.native lib_show/ppx_deriving_"^name^".cmxs")]
      in
      List.iter flags_for ["show"]
    | _ -> ()
    end;
    dispatch_default
  ])
