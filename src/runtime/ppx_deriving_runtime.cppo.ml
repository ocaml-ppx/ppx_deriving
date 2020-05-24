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

#if OCAML_VERSION >= (4, 07, 0)
module Stdlib = Stdlib

include Stdlib

module Result = struct
  (* Type manifest shoud be [('a, 'b) result]:
     - it can't be [Result.t] because [Result] is not defined in 4.07 std-lib
       and the result package just exposes [Result.t] as an alias to [result]
       without re-exporting the constructors
     - it can't be [Result.result] because the [include Stdlib] above makes
       [Result] be [Stdlib.Result] (shadowing the [Result] module from the
       result package), and [Stdlib.Result] does not define [result] (that's
       why we override the [Result] module as the first place. *)
  type ('a, 'b) t = ('a, 'b) result =
    | Ok of 'a
    | Error of 'b

  type ('a, 'b) result = ('a, 'b) t =
    | Ok of 'a
    | Error of 'b
end
#else
module Pervasives = Pervasives
module Stdlib = Pervasives

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

include Pervasives

module Result = struct
  (* the "result" compatibility module defines Result.result as a variant
     and Result.t as an alias *)
  type ('a, 'b) t = ('a, 'b) Result.result =
    | Ok of 'a
    | Error of 'b

  (* ... and we also expose Result.result for backward-compatibility *)
  type ('a, 'b) result = ('a, 'b) Result.result =
    | Ok of 'a
    | Error of 'b
end
#endif

#if OCAML_VERSION < (4, 08, 0)
module Option = struct
  type 'a t = 'a option

  let get o =
    match o with
    | None -> invalid_arg "get"
    | Some x -> x

  let to_result ~none o =
    match o with
    | None -> Result.Error none
    | Some x -> Result.Ok x
end
#endif
