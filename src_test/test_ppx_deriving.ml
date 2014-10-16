open OUnit2

let test_inline ctxt =
  let sort = List.sort [%derive.ord: int * int] in
  assert_equal ~printer:[%derive.show: (int * int) list]
               [(1,1);(2,0);(3,5)] (sort [(2,0);(3,5);(1,1)])

let test_inline_shorthand ctxt =
  assert_equal ~printer:(fun x -> x)
               "[(1, 1); (2, 0)]" ([%show: (int * int) list] [(1,1); (2,0)])

type optional_deriver = string 
[@@deriving missing { optional = true }]

let suite = "Test ppx_deriving" >::: [
    Test_deriving_show.suite;
    Test_deriving_eq.suite;
    Test_deriving_ord.suite;
    Test_deriving_enum.suite;
    Test_deriving_iter.suite;
    Test_deriving_map.suite;
    Test_deriving_fold.suite;
    Test_deriving_create.suite;
    "test_inline"           >:: test_inline;
    "test_inline_shorthand" >:: test_inline_shorthand;
  ]

let _ =
  run_test_tt_main suite
