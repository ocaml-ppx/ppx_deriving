open OUnit2

(* Mostly it is sufficient to test that the derived code compiles. *)

let printer = string_of_int

type a1 = int        [@@deriving ord]
type a2 = int32      [@@deriving ord]
type a3 = int64      [@@deriving ord]
type a4 = nativeint  [@@deriving ord]
type a5 = float      [@@deriving ord]
type a6 = bool       [@@deriving ord]
type a7 = char       [@@deriving ord]
type a8 = string     [@@deriving ord]
type a9 = bytes      [@@deriving ord]
type r  = int ref    [@@deriving ord]
type l  = int list   [@@deriving ord]
type a  = int array  [@@deriving ord]
type o  = int option [@@deriving ord]

let test_simple ctxt =
  assert_equal ~printer  (1) (compare_a1 1 0);
  assert_equal ~printer  (0) (compare_a1 1 1);
  assert_equal ~printer (-1) (compare_a1 1 2)

type v = Foo | Bar of int * string | Baz of string [@@deriving ord]
let test_variant ctxt =
  assert_equal ~printer (1) (compare_v (Baz "b") (Baz "a"));
  assert_equal ~printer (1) (compare_v (Bar (1, "")) Foo);
  assert_equal ~printer (1) (compare_v (Baz "") (Bar (1, "")));
  assert_equal ~printer (-1) (compare_v Foo (Baz ""))

type pv1 = [ `Foo | `Bar of int * string ] [@@deriving ord]
type pv2 = [ `Baz | pv1 ] [@@deriving ord]

type ty = int * string [@@deriving ord]
let test_complex ctxt =
  assert_equal ~printer (0)  (compare_ty (0, "a") (0, "a"));
  assert_equal ~printer (1)  (compare_ty (1, "a") (0, "a"));
  assert_equal ~printer (-1) (compare_ty (0, "a") (1, "a"));
  assert_equal ~printer (-1) (compare_ty (0, "a") (0, "b"));
  assert_equal ~printer (1)  (compare_ty (0, "b") (0, "a"))

type re = {
  f1 : int;
  f2 : string;
} [@@deriving ord]

module M : sig
  type t = int [@@deriving ord]
end = struct
  type t = int [@@deriving ord]
end

type z = M.t [@@deriving ord]

type file = {
  name : string;
  perm : int     [@compare fun a b -> compare b a];
} [@@deriving ord]
let test_custom ctxt =
  assert_equal ~printer (-1) (compare_file { name = ""; perm = 2 }
                                           { name = ""; perm = 1 });
  assert_equal ~printer (1)  (compare_file { name = ""; perm = 1 }
                                           { name = ""; perm = 2 })

type 'a pt = { v : 'a } [@@deriving ord]

let test_placeholder ctxt =
  assert_equal ~printer 0 ([%ord: _] 1 2)

let suite = "Test deriving(ord)" >::: [
    "test_simple"       >:: test_simple;
    "test_variant"      >:: test_variant;
    "test_complex"      >:: test_complex;
    "test_custom"       >:: test_custom;
    "test_placeholder"  >:: test_placeholder;
  ]
