open OUnit2

let suite = "Test ppx_deriving" >::: [
    Test_deriving_show.suite;
  ]

let _ =
  run_test_tt_main suite
