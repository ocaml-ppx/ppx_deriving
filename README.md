[@@deriving]
============

_deriving_ is a library simplifying type-driven code generation on OCaml.

_deriving_ includes a set of useful plugins:

- [show][]
- [eq][]
- [ord][eq]
- [enum][]
- [iter][]
- [map][iter]
- [fold][iter]
- [make][] (`create` also exists, but it remains solely for backwards compatibility)
- [yojson][]
- [protobuf][]

Sponsored by [Evil Martians](http://evilmartians.com).

[show]: #plugin-show
[eq]: #plugins-eq-and-ord
[enum]: #plugin-enum
[iter]: #plugins-iter-map-and-fold
[make]: #plugin-make
[yojson]: https://github.com/ocaml-ppx/ppx_deriving_yojson#usage
[protobuf]: https://github.com/ocaml-ppx/ppx_deriving_protobuf#usage

**Note:** since _deriving_ was released by whitequark in 2014, the OCaml ppx ecosystem has changed a lot. For new projects wishing to create a new deriving plugin, we recommend using [ppxlib](https://github.com/ocaml-ppx/ppxlib) directly. The module [Ppxlib.Deriving](https://ocaml-ppx.github.io/ppxlib/ppxlib/Ppxlib/Deriving/index.html) provide functionality similar to _deriving_, better integrated with ppxlib, and offers a nicer API in some places. _deriving_ is still maintained to keep existing plugins working as well as possible. Although note that the above deprecation note only covers the API and not the plugins (e.g. `ppx_deriving.show`, `ppx_deriving.eq`, ...).

Installation
------------

_deriving_ can be installed via [OPAM](https://opam.ocaml.org):

    opam install ppx_deriving

Buildsystem integration
-----------------------

To use _deriving_, only one modification is needed: you need to require via ocamlfind the package corresponding to the _deriving_ plugin. This will both engage the syntax extension and link in the runtime components of the _deriving_ plugin, if any.

For example, if you are using ocamlbuild, add the following to `_tags` to use the default _deriving_ plugins:

    <src/*>: package(ppx_deriving.std)

With Dune, you should add a `preprocess` directive to your target:

    (executable
      (libraries whatever)
      (preprocess (pps ppx_deriving.show ppx_deriving.ord))
      (name blah))

Dune's `pps` directive allows faster preprocessing by linking the specified preprocessors into a single executable (documented [here](https://readthedocs.org/projects/dune/downloads/pdf/latest/#subsection.5.7.2)). This can significantly speed up compilation on large projects which use many derivers.


If you are using another buildsystem, just make sure it passes `-package ppx_deriving.whatever` to ocamlfind.

Usage
-----

From a user's perspective, _deriving_ is triggered by a `[@@deriving plugin]` annotation attached to a type declaration in structure or signature:

``` ocaml
type point2d = float * float
[@@deriving show]
```

It's possible to invoke several plugins by separating their names with commas:

``` ocaml
type point3d = float * float * float
[@@deriving show, eq]
```

It's possible to pass options to a plugin by appending a record to plugin's name:

``` ocaml
type t = string
[@@deriving yojson { strict = true }]
```

It's possible to make _deriving_ ignore a missing plugin rather than raising an error by passing an `optional = true` option, for example, to enable conditional compilation:

``` ocaml
type addr = string * int
[@@deriving yojson { optional = true }]
```

It's also possible for many plugins to derive a function directly from a type, without declaring it first.

``` ocaml
open OUnit2
let test_list_sort ctxt =
  let sort = List.sort [%derive.ord: int * int] in
  assert_equal ~printer:[%derive.show: (int * int) list]
               [(1,1);(2,0);(3,5)] (sort [(2,0);(3,5);(1,1)])
```

The `[%derive.x:]` syntax can be shortened to `[%x:]`, given that the deriver `x` exists and the payload is a type. If these conditions are not satisfied, the extension node will be left uninterpreted to minimize potential conflicts with other rewriters.

### Working with existing types

At first, it may look like _deriving_ requires complete control of the type declaration. However, a lesser-known OCaml feature allows to derive functions for any existing type. Using `Stdlib.fpclass` as an example, _show_ can be derived as follows:

``` ocaml
# module M = struct
  type myfpclass = fpclass = FP_normal | FP_subnormal | FP_zero | FP_infinite | FP_nan
  [@@deriving show]
end;;
module M :
  sig
    type myfpclass =
      fpclass =
        FP_normal
      | FP_subnormal
      | FP_zero
      | FP_infinite
      | FP_nan
    val pp_myfpclass : Format.formatter -> fpclass -> unit
    val show_myfpclass : fpclass -> string
  end
# M.show_myfpclass FP_normal;;
- : string = "FP_normal"
```

The module is used to demonstrate that `show_myfpclass` really accepts `Stdlib.fpclass`, and not just `M.myfpclass`.

To avoid the need to repeat the type definition, it is possible to use [ppx_import](https://github.com/ocaml-ppx/ppx_import#usage) to automatically pull in the type definition. Attributes can be attached using its `[@with]` replacement feature.

Plugin conventions
------------------

It is expected that all _deriving_ plugins will follow the same conventions, thus simplifying usage.

  * By default, the functions generated by a plugin for a `type foo` are called `fn_foo` or `foo_fn`. However, if the type is called `type t`, the function will be named `foo`. The defaults can be overridden by an `affix = true|false` plugin option.

  * There may be additional attributes attached to the AST. In case of a plugin named `eq` and attributes named `compare` and `skip`, the plugin must recognize all of `compare`, `skip`, `eq.compare`, `eq.skip`, `deriving.eq.compare` and `deriving.eq.skip` annotations. However, if it detects that at least one namespaced (e.g. `eq.compare` or `deriving.eq.compare`) attribute is present, it must not look at any attributes located within a different namespace. As a result, different ppx rewriters can avoid interference even if the attribute names they use overlap.

  * A typical plugin should handle tuples, records, normal and polymorphic variants; builtin types: `int`, `int32`, `int64`, `nativeint`, `float`, `bool`, `char`, `string`, `bytes`, `ref`, `list`, `array`, `option`, `lazy_t` and their `Mod.t` aliases; `Result.result` available since 4.03 or in the `result` opam package; abstract types; and `_`. For builtin types, it should have customizable, sensible default behavior. This default behavior should not be used if a type has a `[@nobuiltin]` attribute attached to it, and the type should be treated as abstract. For abstract types, it should expect to find the functions it would derive itself for that type.

  * If a type is parametric, the generated functions accept an argument for every type variable before all other arguments.

Plugin: show
------------

_show_ derives a function that inspects a value; that is, pretty-prints it with OCaml syntax. However, _show_ offers more insight into the structure of values than the Obj-based pretty printers (e.g. `Printexc`), and more flexibility than the toplevel printer.

``` ocaml
# type t = [ `A | `B of int ] [@@deriving show];;
type t = [ `A | `B of i ]
val pp : Format.formatter -> [< `A | `B of i ] -> unit = <fun>
val show : [< `A | `B of i ] -> string = <fun>
# show (`B 1);;
- : string = "`B (1)"
```

For an abstract type `ty`, _show_ expects to find a `pp_ty` function in the corresponding module.

_show_ allows to specify custom formatters for types to override default behavior. A formatter for type `t` has a type `Format.formatter -> t -> unit`:

``` ocaml
# type file = {
  name : string;
  perm : int     [@printer fun fmt -> fprintf fmt "0o%03o"];
} [@@deriving show];;
# show_file { name = "dir"; perm = 0o755 };;
- : string = "{ name = \"dir\"; perm = 0o755 }"
```

It is also possible to use `[@polyprinter]`. The difference is that for a type `int list`, `[@printer]` should have a signature `formatter -> int list -> unit`, and for `[@polyprinter]` it's `('a -> formatter -> unit) -> formatter -> 'a list -> unit`.

`[@opaque]` is a shorthand for `[@printer fun fmt _ -> Format.pp_print_string fmt "<opaque>"]`.

The function `fprintf` is locally defined in the printer.

By default all constructors are printed with prefix which is dot-separated filename and module path. For example
``` ocaml
# module X = struct type t = C [@@deriving show] end;;
...
# X.(show C);;
- : string = "X.C"
```

This code will create printers which return the string `X.C`, `X` is a module path and `C` is a constructor name. File's name is omitted in the toplevel. To skip all module paths the one needs to derive show with option `with_path` (which defaults to `true`)

``` ocaml
# module X = struct type t = C [@@deriving show { with_path = false }] end;;
...
# X.(show C);;
- : string = "C"
```


Plugins: eq and ord
-------------------

_eq_ derives a function comparing values by semantic equality; structural or physical depending on context. _ord_ derives a function defining a total order for values, returning a negative value if lower, `0` if equal or a positive value if greater. They're similar to `Stdlib.(=)` and `Stdlib.compare`, but are faster, allow to customize the comparison rules, and never raise at runtime. _eq_ and _ord_ are short-circuiting.

``` ocaml
# type t = [ `A | `B of int ] [@@deriving eq, ord];;
type t = [ `A | `B of int ]
val equal : [> `A | `B of int ] -> [> `A | `B of int ] -> bool = <fun>
val compare : [ `A | `B of int ] -> [ `A | `B of int ] -> int = <fun>
# equal `A `A;;
- : bool = true
# equal `A (`B 1);;
- : bool = false
# compare `A `A;;
- : int = 0
# compare (`B 1) (`B 2);;
- : int = -1
```

For variants, _ord_ uses the definition order. For builtin types, properly monomorphized `(=)` is used for _eq_, or corresponding `Mod.compare` function (e.g. `String.compare` for `string`) for _ord_. For an abstract type `ty`, _eq_ and _ord_ expect to find an `equal_ty` or `compare_ty` function in the corresponding module.

_eq_ and _ord_ allow to specify custom comparison functions for types to override default behavior. A comparator for type `t` has a type `t -> t -> bool` for _eq_ or `t -> t -> int` for _ord_. If an _ord_ comparator returns a value outside -1..1 range, the behavior is unspecified.

``` ocaml
# type file = {
  name : string [@equal fun a b -> String.(lowercase a = lowercase b)];
  perm : int    [@compare fun a b -> compare b a]
} [@@deriving eq, ord];;
type file = { name : bytes; perm : int; }
val equal_file : file -> file -> bool = <fun>
val compare_file : file -> file -> int = <fun>
# equal_file { name = "foo"; perm = 0o644 } { name = "Foo"; perm = 0o644 };;
- : bool = true
# compare_file { name = "a"; perm = 0o755 } { name = "a"; perm = 0o644 };;
- : int = -1
```

Plugin: enum
------------

_enum_ is a plugin that treats variants with argument-less constructors as enumerations with an integer value assigned to every constructor. _enum_ derives functions to convert the variants to and from integers, and minimal and maximal integer value.

``` ocaml
# type insn = Const | Push | Pop | Add [@@deriving enum];;
type insn = Const | Push | Pop | Add
val insn_to_enum : insn -> int = <fun>
val insn_of_enum : int -> insn option = <fun>
val min_insn : int = 0
val max_insn : int = 3
# insn_to_enum Pop;;
- : int = 2
# insn_of_enum 3;;
- : insn option = Some Add
```

By default, the integer value associated is `0` for lexically first constructor, and increases by one for every next one. It is possible to set the value explicitly with `[@value 42]`; it will keep increasing from the specified value.

Plugins: iter, map and fold
---------------------------

_iter_, _map_ and _fold_ are three closely related plugins that generate code for traversing polymorphic data structures in lexical order and applying a user-specified action to all values corresponding to type variables.

``` ocaml
# type 'a btree = Node of 'a btree * 'a * 'a btree | Leaf [@@deriving iter, map, fold];;
type 'a btree = Node of 'a btree * 'a * 'a btree | Leaf
val iter_btree : ('a -> unit) -> 'a btree -> unit = <fun>
val map_btree : ('a -> 'b) -> 'a btree -> 'b btree = <fun>
val fold_btree : ('a -> 'b -> 'a) -> 'a -> 'b btree -> 'a = <fun>
# let tree = (Node (Node (Leaf, 0, Leaf), 1, Node (Leaf, 2, Leaf)));;
val tree : int btree = Node (Node (Leaf, 0, Leaf), 1, Node (Leaf, 2, Leaf))
# iter_btree (Printf.printf "%d\n") tree;;
0
1
2
- : unit = ()
# map_btree ((+) 1) tree;;
- : int btree = Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf))
# fold_btree (+) 0 tree;;
- : int = 3
```

Plugin: make
--------------

_make_ is a plugin that generates record constructors. Given a record, a function is generated that accepts all fields as labelled arguments and `()`; alternatively, if one field is specified as `[@main]`, it is accepted last. The fields which have a default value (fields of types `'a option`, `'a list`, and fields with `[@default]` annotation) are mapped to optional arguments; the rest are mandatory. A field of form `xs: ('a * 'a list) [@split]` corresponds to two arguments: mandatory argument `x` and optional argument `xs` with types `'a` and `'a list` correspondingly.

``` ocaml
type record = {
  opt  : int option;
  lst  : int list;
  def  : int [@default 42];
  args : (int * int list) [@split];
  norm : int;
} [@@deriving make];;
val make_record :
  ?opt:int ->
  ?lst:int list ->
  ?def:int ->
  arg:int ->
  ?args:int list ->
  norm:int ->
  unit ->
  record
```

To use _make_ with a set of mutually recursive type definitions, simply attach a
single `[@@deriving make]` attribute and it will derive a `make_*` function for
each record type in the set.

The deriving runtime
--------------------

_deriving_ comes with a small runtime library, the
`Ppx_deriving_runtime` module, whose purpose is to re-export the
modules and types of the standard library that code producers rely
on -- ensuring hygienic code generation.

By emitting code that references to `Ppx_deriving_runtime.Array`
module instead of just `Array`, plugins ensure that they can be used
in environments where the `Array` module is redefined with
incompatible types.

Building ppx drivers
--------------------

By default, _deriving_ dynlinks every plugin, whether invoked as a part of a batch compilation or from the toplevel. If this is unsuitable for you for some reason, it is possible to precompile a ppx rewriter executable that includes several _deriving_ plugins:

```
$ ocamlfind opt -predicates ppx_driver -package ppx_deriving_foo -package ppx_deriving_bar \
                -package ppx_deriving.main -linkpkg -linkall -o ppx_driver
```

Currently, the resulting ppx driver still depends on Dynlink as well as retains the ability to load more plugins.

Developing plugins
------------------

This section only explains the tooling and best practices. Anyone aiming to implement their own _deriving_ plugin is encouraged to explore the existing ones, e.g. [eq](src_plugins/eq/ppx_deriving_eq.cppo.ml) or [show](src_plugins/show/ppx_deriving_show.cppo.ml).

### Tooling and environment

A _deriving_ plugin is packaged as a Findlib library; this library should include a peculiar META file. As an example, let's take a look at a description of a _yojson_ plugin:

```
version = "1.0"
description = "[@@deriving yojson]"
exists_if = "ppx_deriving_yojson.cma"
# The following part affects batch compilation and toplevel.
# The plugin package may require any runtime component it needs.
requires(-ppx_driver) = "ppx_deriving yojson"
ppxopt(-ppx_driver) = "ppx_deriving,./ppx_deriving_yojson.cma"
# The following part affects ppx driver compilation.
requires(ppx_driver) = "ppx_deriving.api"
archive(ppx_driver, byte) = "ppx_deriving_yojson.cma"
archive(ppx_driver, native) = "ppx_deriving_yojson.cmxa"
```

The module(s) provided by the package in the `ppxopt` variable must register the derivers using `Ppx_deriving.register "foo"` during loading. Any number of derivers may be registered; careful registration would allow a _yojson_ deriver to support all three of `[@@deriving yojson]`, `[@@deriving of_yojson]` and `[@@deriving to_yojson]`, as well as `[%derive.of_yojson:]` and `[%derive.to_yojson:]`.

It is possible to test the plugin without installing it by instructing _deriving_ to load it directly; the compiler should be invoked as `ocamlfind c -package ppx_deriving -ppxopt ppx_deriving,src/ppx_deriving_foo.cma ...`. The file extension is replaced with `.cmxs` automatically for native builds. This can be integrated with buildsystem, e.g. for ocamlbuild:

``` ocaml
let () = dispatch (
  function
  | After_rules ->
    (* Assuming files tagged with deriving_foo are already tagged with
       package(ppx_deriving) or anything that uses it, e.g. package(ppx_deriving.std). *)
    flag ["ocaml"; "compile"; "deriving_foo"] &
      S[A"-ppxopt"; A"ppx_deriving,src/ppx_deriving_foo.cma"]
  | _ -> ()
```

Alternatively, you can quickly check the code generated by a ppx rewriter packaged with ocamlfind by running the toplevel as `ocaml -dsource` or `utop -dsource`, which will unparse the rewritten syntax tree into OCaml code and print it before executing.

### Testing plugins

The main ppx_deriving binary can be used to output preprocessed source code in a human-readable form:

```
$ cat test.ml
type foo = A of int | B of float
[@@deriving show]
$ ocamlfind ppx_deriving/ppx_deriving \
    -deriving-plugin `ocamlfind query ppx_deriving`/ppx_deriving_show.cma \
    test.ml
```
``` ocaml
type foo =
  | A of int
  | B of float [@@deriving show]
let rec (pp_foo : Format.formatter -> foo -> Ppx_deriving_runtime.unit) =
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | A a0 ->
            (Format.fprintf fmt "(@[<2>T.A@ ";
             (Format.fprintf fmt "%d") a0;
             Format.fprintf fmt "@])")
        | B a0 ->
            (Format.fprintf fmt "(@[<2>T.B@ ";
             (Format.fprintf fmt "%F") a0;
             Format.fprintf fmt "@])"))
  [@ocaml.warning "-A"])

and show_foo : foo -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_foo x
```

### Goals of the API

_deriving_ is a thin wrapper over the ppx rewriter system. Indeed, it includes very little logic; the goal of the project is 1) to provide common reusable abstractions required by most, if not all, deriving plugins, and 2) encourage the deriving plugins to cooperate and to have as consistent user interface as possible.

As such, _deriving_:

  * Completely defines the syntax of `[@@deriving]` annotation and unifies the plugin discovery mechanism;
  * Provides an unified, strict option parsing API to plugins;
  * Provides helpers for parsing annotations to ensure that the plugins interoperate with each other and the rest of the ecosystem.

### Using the API

Complete API documentation is available [online](http://ocaml-ppx.github.io/ppx_deriving/ppx_deriving/Ppx_deriving).

#### Hygiene

A very important aspect of a syntax extension is **hygiene**. Consider a case where a _deriving_ plugin makes assumptions about the interface provided by the `List` module: it will normally work as expected, but not in case where someone shadows the `List` identifier! This happens quite often in the OCaml ecosystem, e.g. the Jane Street [Core] library encourages developers to use `open Core.Std`.

Additionally, if your _deriving_ plugin inserts user-provided expressions into the generated code, a name you are using internally may accidentally collide with a user-defined name.

With _deriving_, both of these problems are solved in three easy steps:

  * Create a _quoter_:

    ``` ocaml
    let quoter = Ppx_deriving.create_quoter () in
    ...
    ```

  * Pass the user-provided expressions, if any, through the quoter, such as
    by using a helper function:

    ```ocaml
    let attr_custom_fn attrs =
      Ppx_deriving.(attrs |> attr ~deriver "custom_fn" |> Arg.(get_attr ~deriver expr)
                          |> quote ~quoter)
    ```

  * Wrap the generated code:

    ```ocaml
    let expr_of_typ typ =
      let quoter = ...
      and expr = ... in
      Ppx_deriving.sanitize ~quoter expr
    ```

    If the plugin does not accept user-provided expressions, `sanitize expr` could be used
    instead.

#### FAQ

The following is a list of tips for developers trying to use the ppx interface:

  * Module paths overwhelm you? Open all of the following modules, they don't conflict with each other: `Longident`, `Location`, `Asttypes`, `Parsetree`, `Ast_helper`, `Ast_convenience`.
  * Need to insert some ASTs? See [ppx_metaquot](https://github.com/alainfrisch/ppx_tools/blob/master/ppx_metaquot.ml); it is contained in the `ppx_tools.metaquot` package.
  * Need to display an error? Use `Ppx_deriving.raise_errorf ~loc "Cannot derive Foo: (error description)"` ([doc](http://ocaml-ppx.github.io/ppx_deriving/ppx_deriving/Ppx_deriving/index.html#val-raise_errorf)); keep it clear which deriving plugin raised the error!
  * Need to derive a function name from a type name? Use [Ppx_deriving.mangle_type_decl](http://ocaml-ppx.github.io/ppx_deriving/ppx_deriving/Ppx_deriving/index.html#val-mangle_type_decl) and [Ppx_deriving.mangle_lid](http://ocaml-ppx.github.io/ppx_deriving/ppx_deriving/Ppx_deriving/index.html#val-mangle_lid).
  * Need to fetch an attribute from a node? Use `Ppx_deriving.attr ~prefix "foo" nod.nod_attributes` ([doc](http://ocaml-ppx.github.io/ppx_deriving/ppx_deriving/Ppx_deriving/index.html#val-attr)); this takes care of interoperability.
  * Put all functions derived from a set of type declarations into a single `let rec` block; this reflects the always-recursive nature of type definitions.
  * Need to handle polymorphism? Use [Ppx_deriving.poly_fun_of_type_decl](http://ocaml-ppx.github.io/ppx_deriving/ppx_deriving/Ppx_deriving/index.html#val-poly_fun_of_type_decl) for derived functions, [Ppx_deriving.poly_arrow_of_type_decl](http://ocaml-ppx.github.io/ppx_deriving/ppx_deriving/Ppx_deriving/index.html#val-poly_arrow_of_type_decl) for signatures, and [Ppx_deriving.poly_apply_of_type_decl](http://ocaml-ppx.github.io/ppx_deriving/ppx_deriving/Ppx_deriving/index.html#val-poly_apply_of_type_decl) for "forwarding" the arguments corresponding to type variables to another generated function.
  * Need to display a full path to a type, e.g. for an error message? Use [Ppx_deriving.path_of_type_decl](http://ocaml-ppx.github.io/ppx_deriving/ppx_deriving/Ppx_deriving/index.html#val-path_of_type_decl).
  * Need to apply a sequence or a binary operator to variant, tuple or record elements? Use [Ppx_deriving.fold_exprs](http://ocaml-ppx.github.io/ppx_deriving/ppx_deriving/Ppx_deriving/index.html#val-fold_exprs).
  * Don't forget to display an error message if your plugin doesn't parse any options.

License
-------

_deriving_ is distributed under the terms of [MIT license](LICENSE.txt).
