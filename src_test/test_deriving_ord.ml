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

type mrec_variant =
  | MrecFoo of string
  | MrecBar of int

and mrec_variant_list = mrec_variant list
[@@deriving ord]

let test_mrec ctxt =
  assert_equal ~printer (0)   (compare_mrec_variant_list [MrecFoo "foo"; MrecBar 1;]
                                                         [MrecFoo "foo"; MrecBar 1;]);
  assert_equal ~printer (-1)  (compare_mrec_variant_list [MrecFoo "foo"; MrecBar 1;]
                                                         [MrecFoo "foo"; MrecBar 2;]);
  assert_equal ~printer (1)   (compare_mrec_variant_list [MrecFoo "foo"; MrecBar 2;]
                                                         [MrecFoo "foo"; MrecBar 1;])


type e = Bool of be | Plus of e * e | IfE  of (be, e) if_e
and be = True | False | And of be * be | IfB of (be, be) if_e
and ('cond, 'a) if_e = 'cond * 'a * 'a
  [@@deriving ord]

let test_mutualy_recursive ctxt =
  let ce1 = Bool (IfB (True, False, True)) in
  let ce2 = Bool (IfB (True, False, False)) in
  assert_equal ~printer (0) (compare_e ce1 ce1);
  assert_equal ~printer (-1) (compare_e ce1 ce2);
  assert_equal ~printer (1) (compare_e ce2 ce1)

type es =
  | ESBool of bool
  | ESString of string
and bool =
  | Bfoo of int * ((int -> int) [@compare fun _ _ -> 0])
and string =
  | Sfoo of String.t * ((int -> int) [@compare fun _ _ -> 0])
  [@@deriving ord{ allow_std_type_shadowing }]

let test_shadowed_std_type ctxt =
  let e1 = ESBool (Bfoo (1, (+) 1)) in
  let e2 = ESString (Sfoo ("lalala", (+) 3)) in
  assert_equal ~printer (-1) (compare_es e1 e2);
  assert_equal ~printer (1) (compare_es e2 e1);
  assert_equal ~printer 0 (compare_es e1 e1);
  assert_equal ~printer 0 (compare_es e2 e2)

module Warnings = struct
  module W4 = struct
    (* Module does not compile if warning 4 is triggered by the ord
       deriver. *)

    [@@@ocaml.warning "@4"]

    type t =
      | A of int
      | B
          [@@deriving ord]
  end
end

let suite = "Test deriving(ord)" >::: [
    "test_simple"       >:: test_simple;
    "test_variant"      >:: test_variant;
    "test_complex"      >:: test_complex;
    "test_custom"       >:: test_custom;
    "test_placeholder"  >:: test_placeholder;
    "test_mrec"         >:: test_mrec;
    "test_mutualy_recursive" >:: test_mutualy_recursive;
    "test_shadowed_std_type" >:: test_shadowed_std_type;
  ]
