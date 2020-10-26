(** A module collecting all predefined OCaml types, exceptions and
    modules operating on them, so that ppx_deriving plugins operate
    in a well-defined environment. *)

(** {2 Predefined types} *)
type nonrec int = int
type nonrec char = char
type nonrec string = string
type nonrec float = float
type nonrec bool = bool
type nonrec unit = unit
type nonrec exn = exn
type nonrec 'a array = 'a array
type nonrec 'a list = 'a list
type nonrec 'a option = 'a option
type nonrec nativeint = nativeint
type nonrec int32 = int32
type nonrec int64 = int64
type nonrec 'a lazy_t = 'a lazy_t
type nonrec bytes = bytes

(** {2 Predefined modules}
    {3 Operations on predefined types} *)

#if OCAML_VERSION >= (4, 07, 0)
include module type of struct
  include Stdlib
end

module Result : sig
  type ('a, 'b) t = ('a, 'b) result =
    | Ok of 'a
    | Error of 'b

  (* we also expose Result.result for backward-compatibility
     with the Result package! *)
  type ('a, 'b) result = ('a, 'b) t =
    | Ok of 'a
    | Error of 'b
end
#else
module Pervasives = Pervasives

module Stdlib = Pervasives

include module type of struct
  include Pervasives
end

module Char = Char
module String = String
module Printexc = Printexc
module Array = Array
module List = List
module Nativeint = Nativeint
module Int32 = Int32
module Int64 = Int64
module Lazy = Lazy
module Bytes = Bytes

module Hashtbl = Hashtbl
module Queue = Queue
module Stack = Stack
module Set = Set
module Map = Map
module Weak = Weak

module Printf = Printf
module Format = Format
module Buffer = Buffer

module Result : sig
  type ('a, 'b) t = ('a, 'b) Result.result =
    | Ok of 'a
    | Error of 'b

  (* we also expose Result.result for backward-compatibility *)
  type ('a, 'b) result = ('a, 'b) Result.result =
    | Ok of 'a
    | Error of 'b
end

module Option : sig
  type 'a t = 'a option

  val get : 'a t -> 'a

  val to_result : none:'e -> 'a option -> ('a, 'e) result
end
#endif
