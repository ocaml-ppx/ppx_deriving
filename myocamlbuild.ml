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

let () = dispatch (
  function
  | After_rules ->
    pflag ["ocaml"; "compile"] "deriving" (fun names ->
      let plugins = split ',' names in
      let plugins = List.map (fun name -> "src_plugins/ppx_deriving_"^name^".cma") plugins in
      S[A"-ppx"; A("src/ppx_deriving_main.native " ^ (String.concat " " plugins))])

  | _ -> ())
