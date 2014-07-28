#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "ppx_deriving" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.bin ~auto:true "src/ppx_deriving_main" ~dst:"../lib/ppx_deriving/ppx_deriving";
    Pkg.lib ~exts:Exts.module_library "src/ppx_deriving";
    Pkg.lib ~exts:Exts.library "src/ppx_deriving_main";
    Pkg.lib ~exts:Exts.library "src_plugins/ppx_deriving_show";
    Pkg.lib ~exts:Exts.library "src_plugins/ppx_deriving_eq";
    Pkg.lib ~exts:Exts.library "src_plugins/ppx_deriving_ord";
    Pkg.lib ~exts:Exts.library "src_plugins/ppx_deriving_enum";
    Pkg.lib ~exts:Exts.library "src_plugins/ppx_deriving_iter";
    Pkg.lib ~exts:Exts.library "src_plugins/ppx_deriving_map";
    Pkg.lib ~exts:Exts.library "src_plugins/ppx_deriving_fold";
    Pkg.doc "README.md";
    Pkg.doc "LICENSE.txt";
    Pkg.doc "CHANGELOG.md"; ]
