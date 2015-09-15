open Ocamlbuild_plugin

let split delim str =
  let rec loop i last acc =
    if i = String.length str then
      String.sub str last (i - last) :: acc
    else if ((String.get str i) = delim) then
      loop (i + 1) (i + 1) (String.sub str last (i - last) :: acc)
    else
      loop (i + 1) last acc
  in
  List.rev (loop 0 0 [])

let plugin_cmas names =
  split ',' names |>
  List.map (fun name -> "src_plugins/ppx_deriving_" ^ name ^ ".cma") |>
  String.concat " "

let () = dispatch (
  function
  | After_rules ->
    pflag ["ocaml"; "compile"; "ppx_byte"] "deriving" (fun names ->
      S[A"-ppx"; A("src/ppx_deriving_main.byte " ^ (plugin_cmas names))]);
    pflag ["ocaml"; "compile"; "ppx_native"] "deriving" (fun names ->
      S[A"-ppx"; A("src/ppx_deriving_main.native " ^ (plugin_cmas names))]);
    flag ["ocaml"; "link"; "byte"; "use_deriving"] &
      A"src/ppx_deriving_runtime.cma";
    flag ["ocaml"; "link"; "native"; "use_deriving"] &
      A"src/ppx_deriving_runtime.cmxa";

  | _ -> ())
