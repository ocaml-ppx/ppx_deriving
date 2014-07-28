open OUnit2

let suite = "Test ppx_deriving" >::: [
    Test_deriving_show.suite;
    Test_deriving_eq.suite;
    Test_deriving_ord.suite;
    Test_deriving_enum.suite;
    Test_deriving_iter.suite;
  ]

let _ =
  run_test_tt_main suite
