#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  let oc = open_out "src_test/_tags" in
  output_string oc (if Env.native then "<*.ml>: ppx_native" else "<*.ml>: ppx_byte");
  close_out oc

let quote_parens s =
  if Sys.win32 then
    s
  else
    "'" ^ s ^ "'"

let ocamlbuild =
  "ocamlbuild -use-ocamlfind -classic-display -plugin-tag " ^ quote_parens "package(cppo_ocamlbuild)"

let () =
  Pkg.describe "ppx_deriving" ~builder:(`Other (ocamlbuild, "_build")) [
    Pkg.lib "pkg/META";
    Pkg.bin ~auto:true "src/ppx_deriving_main" ~dst:"../lib/ppx_deriving/ppx_deriving";
    Pkg.lib ~exts:Exts.module_library "src/ppx_deriving";
    Pkg.lib ~exts:Exts.library "src/ppx_deriving_main";
    Pkg.lib ~exts:Exts.module_library "src/ppx_deriving_runtime";
    Pkg.lib ~exts:Exts.library "src_plugins/ppx_deriving_show";
    Pkg.lib ~exts:Exts.library "src_plugins/ppx_deriving_eq";
    Pkg.lib ~exts:Exts.library "src_plugins/ppx_deriving_ord";
    Pkg.lib ~exts:Exts.library "src_plugins/ppx_deriving_enum";
    Pkg.lib ~exts:Exts.library "src_plugins/ppx_deriving_iter";
    Pkg.lib ~exts:Exts.library "src_plugins/ppx_deriving_map";
    Pkg.lib ~exts:Exts.library "src_plugins/ppx_deriving_fold";
    Pkg.lib ~exts:Exts.library "src_plugins/ppx_deriving_create";
    Pkg.lib ~exts:Exts.library "src_plugins/ppx_deriving_make";
    Pkg.doc "README.md";
    Pkg.doc "LICENSE.txt";
    Pkg.doc "CHANGELOG.md"; ]
