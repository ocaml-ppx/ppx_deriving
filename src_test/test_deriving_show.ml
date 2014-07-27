open OUnit2

let printer = fun x -> x

type a1 = int        [@@deriving Show]
type a2 = int32      [@@deriving Show]
type a3 = int64      [@@deriving Show]
type a4 = nativeint  [@@deriving Show]
type a5 = float      [@@deriving Show]
type a6 = bool       [@@deriving Show]
type a7 = char       [@@deriving Show]
type a8 = string     [@@deriving Show]
type a9 = bytes      [@@deriving Show]
type l  = int list   [@@deriving Show]
type a  = int array  [@@deriving Show]
type o  = int option [@@deriving Show]
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
  assert_equal ~printer "[1; 2; 3]" (show_l [1;2;3]);
  assert_equal ~printer "[|1; 2; 3|]" (show_a [|1;2;3|]);
  assert_equal ~printer "Some (1)" (show_o (Some 1))

type v = Foo | Bar of int * string | Baz of string [@@deriving Show]
let test_variant ctxt =
  assert_equal ~printer "Test_deriving_show.Foo"              (show_v Foo);
  assert_equal ~printer "Test_deriving_show.Bar (1, \"foo\")" (show_v (Bar (1, "foo")));
  assert_equal ~printer "Test_deriving_show.Baz (\"foo\")"    (show_v (Baz "foo"))

type vn = Foo of int option [@@deriving Show]
let test_variant_nest ctxt =
  assert_equal ~printer "Test_deriving_show.Foo (Some (1))" (show_vn (Foo (Some 1)))

type pv1 = [ `Foo | `Bar of int * string ] [@@deriving Show]
let test_poly ctxt =
  assert_equal ~printer "`Foo"              (show_pv1 `Foo);
  assert_equal ~printer "`Bar (1, \"foo\")" (show_pv1 (`Bar (1, "foo")))

type pv2 = [ `Baz | pv1 ] [@@deriving Show]
let test_poly_inherit ctxt =
  assert_equal ~printer "`Foo" (show_pv2 `Foo);
  assert_equal ~printer "`Baz" (show_pv2 `Baz)

type ty = int * string [@@deriving Show]
let test_tuple ctxt =
  assert_equal ~printer "(1, \"foo\")" (show_ty (1, "foo"))

type r = {
  f1 : int;
  f2 : string;
} [@@deriving Show]
let test_record ctxt =
  assert_equal ~printer "{ Test_deriving_show.f1 = 1; f2 = \"foo\" }" (show_r { f1 = 1; f2 = "foo" })

module M : sig
  type t = A [@@deriving Show]
end = struct
  type t = A [@@deriving Show]
end

let test_module ctxt =
  assert_equal ~printer "Test_deriving_show.M.A" (M.show M.A)

type z = M.t [@@deriving Show]
let test_abstr ctxt =
  assert_equal ~printer "Test_deriving_show.M.A" (show_z M.A)

type file = {
  name : string;
  perm : int     [@printer fun fmt -> Format.fprintf fmt "0o%03o"];
}
[@@deriving Show]
let test_custom ctxt =
  assert_equal ~printer "{ Test_deriving_show.name = \"dir\"; perm = 0o755 }"
                        (show_file { name = "dir"; perm = 0o755 })

type 'a pt = { v : 'a } [@@deriving Show]
let test_parametric ctxt =
  assert_equal ~printer "{ Test_deriving_show.v = 1 }"
                        (show_pt (fun fmt -> Format.fprintf fmt "%d") { v = 1 })

module M' = struct
  type t = M.t = A [@@deriving Show]
end
let test_alias_path ctxt =
  assert_equal ~printer "M.A" (M'.show M'.A)

let suite = "Test deriving(Show)" >::: [
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
  ]
