open OUnit2
open Dnb.Map

let test_rfb _ = 
  assert_equal (rfb "0000" 0) false;
  assert_equal (try (rfb "0a" 1) with _ -> false) false;
  assert_equal (rfb "1000" 0) true;

let test_modify_string _ = 
  assert_equal (modify_string "0000" 0 '1') "1000";
  assert_equal (try (modify_string "0000" 7 '1') with _ -> false) false;

let test_wfb _ =
  assert_equal (wfb "0000" 0 true) "1000";
  assert_equal (try (wfb "0a" 1 true) with _ -> false) false;
  assert_equal (try (wfb "0000" 7 true) with _ -> false) false;
  assert_equal (wfb "1000" 0 false) "0000";

let test_grid_1 = 
  [|
    [|{bin="1111"; content=0}; {bin="0000"; content=0}; {bin="0000"; content=0}|];
    [|{bin="0000"; content=0}; {bin="1111"; content=0}; {bin="0000"; content=0}|];
    [|{bin="0000"; content=0}; {bin="0000"; content=0}; {bin="1111"; content=0}|]
  |];

let test_grid_2 = 
  [|
    [|{bin="1111"; content=0}; {bin="0111"; content=0}; {bin="0100"; content=0}|];
    [|{bin="1010"; content=0}; {bin="1000"; content=0}; {bin="0000"; content=0}|];
    [|{bin="1000"; content=0}; {bin="0000"; content=0}; {bin="0000"; content=0}|]
  |];

let test_fill_map _ = (*TODO : not finished*)
  let f_test_grid = fill_map test_grid in
  Ounit2_extended.assert_custom_equal maps_equal {w=3; h=3; content=f_test_grid} {w=3; h=3; content; test_grid_1}

let test_is_valid_map _ = 
  assert_equal isValid_map {w=3; h=3; content=test_grid_1} false;
  assert_equal isValid_map {w=3; h=3; content=test_grid_2} true;