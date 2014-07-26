open OUnit2

let printer = string_of_int

type va = Aa | Ba | Ca [@@deriving Bounded]
let test_auto ctxt =
  assert_equal ~printer 0 min_va;
  assert_equal ~printer 2 max_va

type vm = Am [@value 1] | Bm [@value 3] | Cm [@@deriving Bounded]
let test_manual ctxt =
  assert_equal ~printer 1 min_vm;
  assert_equal ~printer 4 max_vm

type pv = [ `A | `B | `C ] [@@deriving Bounded]
let test_poly ctxt =
  assert_equal ~printer 0 min_pv;
  assert_equal ~printer 2 max_pv

let suite = "Test deriving(Bounded)" >::: [
    "test_auto"   >:: test_auto;
    "test_manual" >:: test_manual;
    "test_poly"   >:: test_poly;
  ]
