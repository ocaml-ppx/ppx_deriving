open OUnit2

(* Mostly it is sufficient to test that the derived code compiles. *)

let printer = string_of_bool

type a1 = int       [@@deriving Eq]
type a2 = int32     [@@deriving Eq]
type a3 = int64     [@@deriving Eq]
type a4 = nativeint [@@deriving Eq]
type a5 = float     [@@deriving Eq]
type a6 = bool      [@@deriving Eq]
type a7 = char      [@@deriving Eq]
type a8 = string    [@@deriving Eq]
type a9 = bytes     [@@deriving Eq]

let test_simple ctxt =
  assert_equal ~printer true  (equal_a1 1 1);
  assert_equal ~printer false (equal_a1 1 2)

type v = Foo | Bar of int * string | Baz of string [@@deriving Eq]

type pv1 = [ `Foo | `Bar of int * string ] [@@deriving Eq]
type pv2 = [ `Baz | pv1 ] [@@deriving Eq]

type t = int * string [@@deriving Eq]

type r = {
  f1 : int;
  f2 : string;
} [@@deriving Eq]

module M : sig
  type t = int [@@deriving Eq]
end = struct
  type t = int [@@deriving Eq]
end

type z = M.t [@@deriving Eq]

type file = {
  name : string;
  perm : int     [@equal (<>)];
} [@@deriving Eq]
let test_custom ctxt =
  assert_equal ~printer false (equal_file { name = ""; perm = 1 }
                                          { name = ""; perm = 1 });
  assert_equal ~printer true  (equal_file { name = ""; perm = 1 }
                                          { name = ""; perm = 2 })

type 'a pt = { v : 'a } [@@deriving Eq]

let suite = "Test deriving(Eq)" >::: [
    "test_simple" >:: test_simple;
    "test_custom" >:: test_custom;
  ]
