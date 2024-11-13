open OUnit2
open Test_game
open Test_map

let () = 
  let sequence = "test sequence" >::: [
    (*tests map module*)
    "test_side_of_int" >:: test_side_of_int;
    "test_int_of_side" >:: test_int_of_side;
    "test_is_full_cell" >:: test_is_full_cell;
    "test_convert_to_block" >:: test_convert_to_block;
    "test_is_full" >:: test_is_full;
    "test_is_legale" >:: test_is_legale;
    "test_empty_map" >:: test_empty_map;
    "test_random_map" >:: test_random_map;
    "test_perlin_map" >:: test_perlin_map;
    
    QCheck_ounit.to_ounit2_test (test_maps_equal random_map_gen); 
    QCheck_ounit.to_ounit2_test (test_maps_equal perlin_map_gen); 
    QCheck_ounit.to_ounit2_test prop_map_dimensions; 

    

    (* tests game module *)
    (*"test_update_score" >:: test_update_score;*)
    "test_play_of_string" >:: test_play_of_string;

    "test_act" >:: test_act;

    QCheck_ounit.to_ounit2_test (test_get_player_id ()); 

  ] in run_test_tt_main sequence