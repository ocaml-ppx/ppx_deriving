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

(** [attr ~prefix name attrs] searches for an attribute [\[\@prefix.name\]]
    in [attrs] if any attribute with name starting with [\@prefix] exists,
    or [\[\@name\]] otherwise. *)
val attr : prefix:string -> string -> attributes -> attribute option
