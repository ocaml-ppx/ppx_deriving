Changelog
=========

0.2
---

  * Added Ppx_deriving.{catch,expand_path,path_of_type_decl}.
  * Change Ppx_deriving.deriver signature to pass ~options and ~path as labeled arguments.
  * Show: include full module path in output.
  * Show: correctly show nested constructors.
  * Show: point to original module for types with manifest and declaration.
  * Show, Eq, Ord: add support for list, array, option.

0.1
---

  * Initial release.
