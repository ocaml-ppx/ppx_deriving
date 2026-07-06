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

include module type of struct
  include Stdlib
end

module Stdlib = Stdlib

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
