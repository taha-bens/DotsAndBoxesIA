open OUnit2

let () = 
  let sequence = "test sequence" >::: [
    (*tests map module*)
    (*"test_rfb" >:: Test_map.test_rfb;
    "test_modify_string" >:: Test_map.test_modify_string;
    "test_wfb" >:: Test_map.test_wfb;
    "test_fill_map" >:: Test_map.test_fill_map;
    "test_is_valid_map" >:: Test_map.test_is_valid_map;
    *)


    (* tests game module *)
    "test_nth_letter" >:: Test_game.test_nth_letter;
    QCheck_ounit.to_ounit2_test (Test_game.test_check_move Test_game.random_map_gen);


  ] in run_test_tt_main sequence