(** Public API of [ppx_deriving] executable. *)

open Parsetree

(** {2 Registration} *)

(** A type of deriving functions. A deriving function accepts a list of
    [~options] and a type declaration item ([type t = .. and t' = ..]), and
    returns a list of items to be appended after the type declaration item
    in structure and signature.

    [~path] contains the stack of modules for the type currently
    being processed, with [[]] for toplevel phrases.

    This value is based on [Location.input_name] and thus contains incorrect
    information when used with -for-pack. See PR6497. *)
type deriver = options:(string * expression) list ->
               path:string list ->
               type_declaration list -> structure * signature

(** [register name fn] registers a deriving function [fn] as [name].
    For automatic dynlinking to work, a module [Foo] must register itself
    with name ["Foo"]. *)
val register : string -> deriver -> unit

(** [lookup name] looks up a deriving function called [name]. *)
val lookup : string -> deriver option

(** {2 Error handling} *)

(** [raise_error] is a shorthand for raising [Location.Error] with the result
    of [Location.errorf]. *)
val raise_errorf : ?sub:Location.error list -> ?if_highlight:string ->
                   ?loc:Location.t -> ('a, unit, string, 'b) format4 -> 'a

(** [catch f] converts any exceptions registered with [Location.register_error_of_exn]
    to [[%%ocaml.error]] extension nodes. *)
val catch : (unit -> structure) -> structure

(** [string_of_core_type typ] unparses [typ], omitting any attributes. *)
val string_of_core_type : Parsetree.core_type -> string

(** {2 AST manipulation} *)

(** [expand_path name] returns [name] with the [path] module path prepended,
    e.g. [expand_path ["Foo";"M"] "t"] = ["Foo.M.t"] and [expand_path [] "t"] = ["t"] *)
val expand_path : path:string list -> string -> string

(** [path_of_type_decl ~path type_] returns [path] if [type_] does not have a manifest
    or the manifest is not a constructor, and the module path of manifest otherwise.

    [path_of_type_decl] is useful when determining the canonical path location
    of fields and constructors; e.g. for [type bar = M.foo = A | B], it will return
    [["M"]]. *)
val path_of_type_decl : path:string list -> type_declaration -> string list

(** [mangle_lid ~prefix ~suffix lid] adds [prefix] and [suffix] to the last
    component of [lid], e.g. [mangle_lid ~prefix:"pp_"] applied to [A.foo]
    results in [A.pp_foo]. *)
val mangle_lid : ?prefix:string -> ?suffix:string -> Longident.t -> Longident.t

(** [attr ~prefix name attrs] searches for an attribute [\[\@deriving.prefix.name\]]
    in [attrs] if any attribute with name starting with [\@deriving.prefix] exists,
    or [\[\@prefix.name\]] if any attribute with name starting with [\@prefix] exists,
    or [\[\@name\]] otherwise. *)
val attr : prefix:string -> string -> attributes -> attribute option

(** [poly_fun_of_type_decl type_ expr] wraps [expr] into [fun poly_N -> ...] for every
    type parameter ['N] present in [type_]. For example, if [type_] refers to
    [type ('a, 'b) map], [expr] will be wrapped into [fun poly_a poly_b -> [%e expr]].

    [_] parameters are ignored.  *)
val poly_fun_of_type_decl : type_declaration -> expression -> expression

(** [poly_apply_of_type_decl type_ expr] wraps [expr] into [expr poly_N] for every
    type parameter ['N] present in [type_]. For example, if [type_] refers to
    [type ('a, 'b) map], [expr] will be wrapped into [[%e expr] poly_a poly_b].

    [_] parameters are ignored. *)
val poly_apply_of_type_decl : type_declaration -> expression -> expression

(** [poly_arrow_of_type_decl fn type_ typ] wraps [typ] in an arrow with [fn [%type: 'N]]
    as argument for every type parameter ['N] present in [type_]. For example, if
    [type_] refers to [type ('a, 'b) map] and [fn] is [fun var -> [%type: [%t var] -> string]],
    [typ] will be wrapped into [('a -> string) -> ('b -> string) -> [%t typ]].

    [_] parameters are ignored. *)
val poly_arrow_of_type_decl : (core_type -> core_type) ->
                              type_declaration -> core_type -> core_type

(** [typ_of_type_decl type_] constructs type [('a, 'b, ...) t] for type declaration
    [type ('a, 'b, ...) t = ...]. *)
val typ_of_type_decl : type_declaration -> core_type

(** [fold_exprs ~unit fn exprs] folds [exprs] using head of [exprs] as initial
    accumulator value, or [unit] if [exprs = []].

    See also {!seq_reduce} and {!binop_reduce}. *)
val fold_exprs : ?unit:expression -> (expression -> expression -> expression) ->
                 expression list -> expression

(** [seq_reduce] ≡ [fun x a b -> [%expr [%e a]; [%e x]; [%e b]]]. *)
val seq_reduce : expression -> expression -> expression -> expression

(** [binop_reduce] ≡ [fun x a b -> [%expr [%e x] [%e a] [%e b]]]. *)
val binop_reduce : expression -> expression -> expression -> expression
