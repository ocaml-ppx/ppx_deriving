open OUnit2

let test_shorthand ctxt =
  let sort = List.sort [%derive.ord: int * int] in
  assert_equal ~printer:[%derive.show: (int * int) list]
               [(1,1);(2,0);(3,5)] (sort [(2,0);(3,5);(1,1)])

let suite = "Test ppx_deriving" >::: [
    Test_deriving_show.suite;
    Test_deriving_eq.suite;
    Test_deriving_ord.suite;
    Test_deriving_enum.suite;
    Test_deriving_iter.suite;
    Test_deriving_map.suite;
    Test_deriving_fold.suite;
    "test_shorthand" >:: test_shorthand;
  ]

let _ =
  run_test_tt_main suite
