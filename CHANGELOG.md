Changelog
=========

0.3
---

  * Show, Eq, Ord, Iter, Fold: handle ref.
  * Show: include break hints in format strings.
  * Show: pull fprintf into local environment.
  * Show: add [@polyprinter].

0.2
---

  * New plugins: Enum, Iter, Map, Fold.
  * All plugins: don't concatenate affix if type is named `t`.
  * Add [%derive.Foo:] shorthand.
  * Show, Eq, Ord: add support for list, array, option.
  * Show: include full module path in output, including for types with manifest.
  * A lot of changes in Ppx_deriving interface.

0.1
---

  * Initial release.
