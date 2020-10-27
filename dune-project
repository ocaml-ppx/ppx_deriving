(lang dune 2.7)
(name ppx_deriving)
(version 5.2)

(formatting disabled)
(generate_opam_files true)
(implicit_transitive_deps true) ; TODO: change to false

(license MIT)
(maintainers "whitequark <whitequark@whitequark.org>")
(authors "whitequark <whitequark@whitequark.org>")
(source (github ocaml-ppx/ppx_deriving))
(documentation "https://ocaml-ppx.github.io/ppx_deriving")

(package
  (name ppx_deriving)
  (synopsis "Type-driven code generation for OCaml")
  (description "ppx_deriving provides common infrastructure for generating
code based on type definitions, and a set of useful plugins
for common tasks.
")
  (depends
    (ocaml (>= 4.05))
    (cppo :build)
    ppx_derivers
    (ppxlib (and (>= 0.16.0) (< 0.18.0)))
    (ounit2 :with-test))
  (tags (syntax)))
