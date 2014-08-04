open OUnit2

(* Mostly it is sufficient to test that the derived code compiles. *)

let printer = string_of_int

type a1 = int        [@@deriving Ord]
type a2 = int32      [@@deriving Ord]
type a3 = int64      [@@deriving Ord]
type a4 = nativeint  [@@deriving Ord]
type a5 = float      [@@deriving Ord]
type a6 = bool       [@@deriving Ord]
type a7 = char       [@@deriving Ord]
type a8 = string     [@@deriving Ord]
type a9 = bytes      [@@deriving Ord]
type r  = int ref    [@@deriving Ord]
type l  = int list   [@@deriving Ord]
type a  = int array  [@@deriving Ord]
type o  = int option [@@deriving Ord]

let test_simple ctxt =
  assert_equal ~printer  (1) (compare_a1 1 0);
  assert_equal ~printer  (0) (compare_a1 1 1);
  assert_equal ~printer (-1) (compare_a1 1 2)

type v = Foo | Bar of int * string | Baz of string [@@deriving Ord]
let test_variant ctxt =
  assert_equal ~printer (1) (compare_v (Baz "b") (Baz "a"));
  assert_equal ~printer (1) (compare_v (Bar (1, "")) Foo);
  assert_equal ~printer (1) (compare_v (Baz "") (Bar (1, "")));
  assert_equal ~printer (-1) (compare_v Foo (Baz ""))

type pv1 = [ `Foo | `Bar of int * string ] [@@deriving Ord]
type pv2 = [ `Baz | pv1 ] [@@deriving Ord]

type ty = int * string [@@deriving Ord]
let test_complex ctxt =
  assert_equal ~printer (0)  (compare_ty (0, "a") (0, "a"));
  assert_equal ~printer (1)  (compare_ty (1, "a") (0, "a"));
  assert_equal ~printer (-1) (compare_ty (0, "a") (1, "a"));
  assert_equal ~printer (-1) (compare_ty (0, "a") (0, "b"));
  assert_equal ~printer (1)  (compare_ty (0, "b") (0, "a"))

type re = {
  f1 : int;
  f2 : string;
} [@@deriving Ord]

module M : sig
  type t = int [@@deriving Ord]
end = struct
  type t = int [@@deriving Ord]
end

type z = M.t [@@deriving Ord]

type file = {
  name : string;
  perm : int     [@compare fun a b -> compare b a];
} [@@deriving Ord]
let test_custom ctxt =
  assert_equal ~printer (-1) (compare_file { name = ""; perm = 2 }
                                           { name = ""; perm = 1 });
  assert_equal ~printer (1)  (compare_file { name = ""; perm = 1 }
                                           { name = ""; perm = 2 })

type 'a pt = { v : 'a } [@@deriving Ord]

let suite = "Test deriving(Ord)" >::: [
    "test_simple"  >:: test_simple;
    "test_variant" >:: test_variant;
    "test_complex" >:: test_complex;
    "test_custom"  >:: test_custom;
  ]
