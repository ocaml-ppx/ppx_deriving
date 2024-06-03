unreleased
----------

* Add show @printer support for polymorphic variants
  #286
  (Simmo Saan and Guillaume Huysmans)

6.0.2
-----

* Fix ordering of derived `make`'s arguments
  #285
  (@NathanReb)

6.0.1 (17/04/2024) (aborted, not on opam)
-----------------------------------------

* Fix the unintentional removal of `Ppx_deriving_runtime.Result` in #279
  #282
  (@NathanReb)

6.0.0 (15/04/2024) (aborted, not on opam)
----------------------------

* Fix a bug in `[@@deriving make]` that caused errors when it was used on a set
  of type declarations containing at least one non record type.
  #281
  (@NathanReb)

* Embed errors instead of raising exceptions when generating code
  with `ppx_deriving.make`
  #281
  (@NathanReb)

* Remove `[%derive.iter ...]`, `[%derive.map ...]` and `[%derive.fold ...]`
  extensions
  #278
  (Simmo Saan)

* Port standard plugins to ppxlib registration and attributes
  #263
  (Simmo Saan)

* Optimize forwarding in eq and ord plugins
  #252
  (Simmo Saan)

* Delegate quoter to ppxlib
  #263
  (Simmo Saan)

* Introduce `Ppx_deriving_runtime.Stdlib` with OCaml >= 4.07.
  This module already exists in OCaml < 4.07 but was missing otherwise.
  #258
  (Kate Deplaix)

5.2.1 (02/02/2021)
------------------

* Allow Ast_convenience's functions to be given a full path ident (e.g. M.ident)
  #248
  (Kate Deplaix)

* Add a deprecation notice of the API in the README.
  The next step of the deprecation is going to be in the form of a
  [@@@ocaml.deprecated ...] alert on the API module and the reimplementation of
  the individual plugins using the ppxlib API.
  (Kate Deplaix and Gabriel Scherer)

5.2 (25/11/2020)
----------------

* Update to ppxlib 0.20.0
  #237 #239 #243 #245
  (Kate Deplaix, Jérémie Dimino, Thierry Martinez, Gabriel Scherer)

* Upgrade testsuite from ounit to ounit2
  #241
  (Kate Deplaix)

* (almost) use the set of standard flags from dune
  #246
  (Kate Deplaix)

5.1 (26/10/2020)
----------------

* Update to ppxlib 0.15.0 #235
  (Kate Deplaix)

5.0 (26/10/2020)
----------------

* Migrate to ppxlib #206, #210
  (Anton Kochkov, Gabriel Scherer, Thierry Martinez)

4.5
---

* Add support for OCaml 4.11.
  - `Ppx_deriving.string_of_{constant,expression}_opt` to destruct
    `Pconst_string` in a version-independent way
  #220, #222
  (Kate Deplaix, Thierry Martinez, review by Gabriel Scherer)

* Stronger type equalities in `Ppx_deriving_runtime` (for instance,
  `Ppx_deriving_runtime.result` and `Result.result` are now compatible with
  all OCaml versions)
  #223, #225
  (Thierry Martinez, review by Gabriel Scherer)

* `Ppx_deriving_runtime.Option` compatibility module
  #222
  (Thierry Martinez, review by Gabriel Scherer)

4.4.1
-----

* Add support for OCaml 4.10
  #211
  (Kate Deplaix, review by Gabriel Scherer)

4.4
---

* Restore support for OCaml 4.02.3
  #188
  (ELLIOTTCABLE)
* workaround Location.input_filename being empty
  when using reason-language-server
  #196
  (Ryan Artecona)
* Add support for OCaml 4.08.0
  #193, #197, #200
  (Gabriel Scherer)

4.3
---

* use Format through Ppx_deriving_runtime to avoid deprecation warning
  for users of JaneStreet Base
  (Stephen Bastians and Gabriel Scherer, review by whitequark)
* silence a ambiguous-field warning (41) in generated code
  #163
  (Étienne Millon, review by Gabriel Scherer)
* use dune
  #170
  (Rudi Grinberg, Jérémie Dimino)
* silence an unused-value warning for show
  #179
  (Nathan Rebours)

4.2.1
-----

  * Add support for OCaml 4.06.0
    #154, #155, #156, #159
    (Gabriel Scherer, Fabian, Leonid Rozenberg)
  * Consider { with_path = false } when printing record fields
    #157
    (François Pottier)

4.2
---

  * Add support for OCaml 4.05.0.
  * Use the `ocaml-migrate-parsetree` library to support multiple
    versions of OCaml.
  * Fix comparison order of fields in records (#136).
  * Silence an `unused rec flag` warning in generated code (#137).
  * Monomorphize comparison function for builtin types (#115)
  * Raise an error when `type nonrec` is encountered (#116).
  * Display an error message when dynamic package loading fails.
  * Add a `with_path` option to `@@deriving` to skip the module path
    in generated code (#120).

The homepage for the project has now moved to:
<https://github.com/ocaml-ppx/ppx_deriving>

4.1
---

  * Fix type error with inheritied polymorphic variant type in
    [@@deriving map].
  * Fix incorrect handling of multi-argument constructors in
    [@@deriving show].
  * Add API hooks for ppx_type_conv.

4.0
---

  * Show, eq, ord, map, iter, fold: add support for `Result.result`.
  * Ppx_deriving.Arg: use Result.result instead of polymorphic variants.
  * Ppx_deriving.sanitize: parameterize over an opened module.
  * Add support for `[@@deriving]` in module type declarations.
  * Add support for loading findlib packages instead of just files in
    ppx_deriving_main.
  * Treat types explicitly qualified with Pervasives also as builtin.
  * Compatibility with statically linked ppx drivers.

3.1
---

  * Show, eq, ord: hygienically invoke functions from referenced modules
    (such as X.pp for X.t when deriving show) to coexist with modules
    shadowing ones from standard library.
  * Iter, map, fold: hygienically invoke List and Array functions.

3.0
---

  * Implement hygiene: Ppx_deriving.{create_quoter,quote,sanitize,with_quoter}.
  * Show, eq, ord: add support for `lazy_t`.
  * Add support for `[@nobuiltin]` attribute.
  * Add Ppx_deriving.hash_variant.
  * Remove allow_std_type_shadowing option.
  * Remove Ppx_deriving.extract_typename_of_type_group.

2.1
---

  * Fix breakage occurring with 4.02.2 w.r.t record labels
  * Fix prefixed attribute names (`[@deriving.foo.attr]` and `[@foo.attr]`).
  * Add allow_std_type_shadowing option for eq and show.

2.0
---

  * Add support for open types.

1.1
---

  * New plugin: create.
  * Show, eq, ord: handle `_`.
  * Show, eq, ord, map, iter, fold: handle inheriting from a parametric
    polymorphic variant type.
  * Make `Ppx_deriving.poly_{fun,arrow}_of_type_decl` construct functions
    in correct order. This also fixes all derivers with types with
    more than one parameter.
  * Add `Ppx_deriving.fold_{left,right}_type_decl`.

1.0
---

  * Make deriver names lowercase.
  * Remove Findlib+dynlink integration. All derivers must now be
    explicitly required.
  * Allow shortening [%derive.x:] to [%x:] when deriver x exists.
  * Make `Ppx_deriving.core_type` field optional to allow ignoring
    unsupported [%x:] shorthands.
  * Add support for [@@deriving foo { optional = true }] that does
    not error out if foo is missing, useful for optional dependencies.
  * Rename ~name and ~prefix of `Ppx_deriving.attr` and
    `Ppx_deriving.Arg.payload` to `~deriver`.
  * Renamed `Ppx_deriving.Arg.payload` to `get_attr`.
  * Add `Ppx_deriving.Arg.get_expr` and `get_flag`.

0.3
---

  * Show, Eq, Ord, Iter, Fold: handle ref.
  * Show: handle functions.
  * Show: include break hints in format strings.
  * Show: pull fprintf into local environment.
  * Show: add `[@polyprinter]` and `[@opaque]`.
  * Add `Ppx_deriving.Arg.expr`.

0.2
---

  * New plugins: Enum, Iter, Map, Fold.
  * All plugins: don't concatenate affix if type is named `t`.
  * Add `[%derive.Foo:]` shorthand.
  * Show, Eq, Ord: add support for list, array, option.
  * Show: include full module path in output, including for types with manifest.
  * A lot of changes in `Ppx_deriving interface`.

0.1
---

  * Initial release.
