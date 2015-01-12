open OUnit2

let printer = fun x -> x

type a1 = int        [@@deriving show]
type a2 = int32      [@@deriving show]
type a3 = int64      [@@deriving show]
type a4 = nativeint  [@@deriving show]
type a5 = float      [@@deriving show]
type a6 = bool       [@@deriving show]
type a7 = char       [@@deriving show]
type a8 = string     [@@deriving show]
type a9 = bytes      [@@deriving show]
type r  = int ref    [@@deriving show]
type l  = int list   [@@deriving show]
type a  = int array  [@@deriving show]
type o  = int option [@@deriving show]
type f  = int -> int [@@deriving show]
let test_alias ctxt =
  assert_equal ~printer "1"       (show_a1 1);
  assert_equal ~printer "1l"      (show_a2 1l);
  assert_equal ~printer "1L"      (show_a3 1L);
  assert_equal ~printer "1n"      (show_a4 1n);
  assert_equal ~printer "1."      (show_a5 1.);
  assert_equal ~printer "true"    (show_a6 true);
  assert_equal ~printer "'a'"     (show_a7 'a');
  assert_equal ~printer "\"foo\"" (show_a8 "foo");
  assert_equal ~printer "\"foo\"" (show_a9 (Bytes.of_string "foo"));
  assert_equal ~printer "ref (1)" (show_r (ref 1));
  assert_equal ~printer "[1; 2; 3]" (show_l [1;2;3]);
  assert_equal ~printer "[|1; 2; 3|]" (show_a [|1;2;3|]);
  assert_equal ~printer "(Some 1)" (show_o (Some 1));
  assert_equal ~printer "<fun>"   (show_f (fun x -> x))

type v = Foo | Bar of int * string | Baz of string [@@deriving show]
let test_variant ctxt =
  assert_equal ~printer "Test_deriving_show.Foo"              (show_v Foo);
  assert_equal ~printer "Test_deriving_show.Bar (1, \"foo\")" (show_v (Bar (1, "foo")));
  assert_equal ~printer "(Test_deriving_show.Baz \"foo\")"    (show_v (Baz "foo"))

type vn = Foo of int option [@@deriving show]
let test_variant_nest ctxt =
  assert_equal ~printer "(Test_deriving_show.Foo (Some 1))" (show_vn (Foo (Some 1)))

type pv1 = [ `Foo | `Bar of int * string ] [@@deriving show]
let test_poly ctxt =
  assert_equal ~printer "`Foo"                (show_pv1 `Foo);
  assert_equal ~printer "`Bar ((1, \"foo\"))" (show_pv1 (`Bar (1, "foo")))

type pv2 = [ `Baz | pv1 ] [@@deriving show]
let test_poly_inherit ctxt =
  assert_equal ~printer "`Foo" (show_pv2 `Foo);
  assert_equal ~printer "`Baz" (show_pv2 `Baz)

type ty = int * string [@@deriving show]
let test_tuple ctxt =
  assert_equal ~printer "(1, \"foo\")" (show_ty (1, "foo"))

type re = {
  f1 : int;
  f2 : string;
  f3 : float [@opaque];
} [@@deriving show]
let test_record ctxt =
  assert_equal ~printer "{ Test_deriving_show.f1 = 1; f2 = \"foo\"; f3 = <opaque> }"
                        (show_re { f1 = 1; f2 = "foo"; f3 = 1.0 })

module M : sig
  type t = A [@@deriving show]
end = struct
  type t = A [@@deriving show]
end

let test_module ctxt =
  assert_equal ~printer "Test_deriving_show.M.A" (M.show M.A)

type z = M.t [@@deriving show]
let test_abstr ctxt =
  assert_equal ~printer "Test_deriving_show.M.A" (show_z M.A)

type file = {
  name : string;
  perm : int     [@printer fun fmt -> Format.fprintf fmt "0o%03o"];
}
[@@deriving show]
let test_custom ctxt =
  assert_equal ~printer "{ Test_deriving_show.name = \"dir\"; perm = 0o755 }"
                        (show_file { name = "dir"; perm = 0o755 })

type 'a pt = { v : 'a } [@@deriving show]
let test_parametric ctxt =
  assert_equal ~printer "{ Test_deriving_show.v = 1 }"
                        (show_pt (fun fmt -> Format.fprintf fmt "%d") { v = 1 })

type 'a btree = Node of 'a btree * 'a * 'a btree | Leaf
[@@deriving show]

module M' = struct
  type t = M.t = A [@@deriving show]
end
let test_alias_path ctxt =
  assert_equal ~printer "M.A" (M'.show M'.A)

let print_hi = fun fmt _ -> Format.fprintf fmt "hi!"
type polypr = (string [@printer print_hi]) btree [@polyprinter pp_btree]
[@@deriving show]
let test_polypr ctxt =
  assert_equal ~printer "Test_deriving_show.Node (Test_deriving_show.Leaf, hi!,\n\
                        \  Test_deriving_show.Leaf)"
                        (show_polypr (Node (Leaf, "x", Leaf)))

let test_placeholder ctxt =
  assert_equal ~printer "_" ([%show: _] 1)

module rec RecFoo : sig
  type ('a,'b) t = ('b, 'a) RecBar.t [@@deriving show]
end = struct
  type ('a,'b) t = ('b,'a) RecBar.t [@@deriving show]
end
and RecBar : sig
  type ('b, 'a) t = 'b * 'a [@@deriving show]
end = struct
  type ('b,'a) t = 'b * 'a [@@deriving show]
end

let suite = "Test deriving(show)" >::: [
    "test_alias"        >:: test_alias;
    "test_variant"      >:: test_variant;
    "test_variant_nest" >:: test_variant_nest;
    "test_tuple"        >:: test_tuple;
    "test_poly"         >:: test_poly;
    "test_poly_inherit" >:: test_poly_inherit;
    "test_record"       >:: test_record;
    "test_abstr"        >:: test_abstr;
    "test_custom"       >:: test_custom;
    "test_parametric"   >:: test_parametric;
    "test_alias_path"   >:: test_alias_path;
    "test_polypr"       >:: test_polypr;
    "test_placeholder"  >:: test_placeholder;
  ]
