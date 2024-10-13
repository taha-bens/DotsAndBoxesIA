open OUnit2

let test_a _ = 
  assert_equal 4 4

let test_b _ =
  assert_equal 1 1;
  assert_equal 2 2

let () = 
  let sequence = "test sequence" >::: [
    "test_a" >:: test_a;
    "test_b" >:: test_b
  ] in run_test_tt_main sequence