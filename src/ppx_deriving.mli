(** Public API of [ppx_deriving] executable. *)

open Parsetree

(** {2 Registration} *)

(** A type of deriving functions. A deriving function accepts a list of
    options and a type declaration item ([type t = .. and t' = ..]), and
    returns a list of items to be appended after the type declaration item
    in structure and signature. *)
type deriver = (string * expression) list -> type_declaration list -> structure * signature

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
                   ?loc:Location.t -> ('a, unit, bytes, 'b) format4 -> 'a

(** [string_of_core_type typ] unparses [typ], omitting any attributes. *)
val string_of_core_type : Parsetree.core_type -> string

(** {2 AST manipulation} *)

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

(** [poly_arrow_of_type_decl ~fn type_ typ] wraps [typ] in an arrow with [fn "N"]
    as argument for every type parameter ['N] present in [type_]. For example, if
    [type_] refers to [type ('a, 'b) map] and [fn] is
    [fun name -> [%type: [%t Typ.constr (lid name) []] -> string]],
    [typ] will be wrapped into [('a -> string) -> ('b -> string) -> [%t typ]].

    [_] parameters are ignored. *)
val poly_arrow_of_type_decl : fn:(string -> core_type) ->
                               type_declaration -> core_type -> core_type
