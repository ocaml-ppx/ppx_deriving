Changelog
=========

0.2
---

  * New plugins: Enum.
  * Added Ppx_deriving.(expand_path,path_of_type_decl,function_name,catch,Arg).
  * Renamed Ppx.deriving.(typ_of_type_decl→core_type_of_type_decl).
  * Change Ppx_deriving.deriver signature to pass ~options and ~path as labeled arguments.
  * Show: include full module path in output.
  * Show: correctly show nested constructors.
  * Show: point to original module for types with manifest and declaration.
  * Show, Eq, Ord: add support for list, array, option.
  * All plugins: don't concatenate affix if type is named `t`.

0.1
---

  * Initial release.
