open OUnit2

let suite = "Test ppx_deriving" >::: [
    Test_deriving_show.suite;
    Test_deriving_eq.suite;
    Test_deriving_ord.suite;
  ]

let _ =
  run_test_tt_main suite
