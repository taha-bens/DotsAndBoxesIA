open OUnit2
open Dnb.Map
open QCheck


(* map d'une seule cellule *)
let map1_template n o s e = {width=1;height=1;content=[|[|{walls= [|ref n; ref o; ref s; ref e|];ctype = Void}|]|]}
  

(* générateur d'entier entre 1 et 15 correspondants aux dimensions de la map *)
let dim_gen = QCheck.Gen.int_range 1 15

(* générateur de map aléatoire
  On suppose que Dnb.Map.random_map est correct *)
let random_map_gen = Gen.map2 (fun w h -> Dnb.Map.random_map w h) dim_gen dim_gen

(* générateur de map perlin
  On suppose que Dnb.Map.perlin_map est correct *)
let perlin_map_gen = Gen.map2 (fun w h -> Dnb.Map.perlin_map w h) dim_gen dim_gen

let test_maps_equal map_gen = 
  QCheck.Test.make ~count:10 ~name:"test_maps_equal"
  (make ~print: (fun _ -> "test_maps_equal")
  map_gen)
  (fun m -> maps_equal m m cells_equal)

let test_side_of_int _ = 
  assert_equal (side_of_int 0) N;
  assert_equal (side_of_int 1) O;
  assert_equal (side_of_int 2) S;
  assert_equal (side_of_int 3) E;
  assert_equal (side_of_int 100) E

let test_int_of_side _ = 
  assert_equal (int_of_side N) 0;
  assert_equal (int_of_side O) 1;
  assert_equal (int_of_side S) 2;
  assert_equal (int_of_side E) 3	


let test_is_full_cell _ =
  assert_equal (is_full_cell ({walls = (Array.init 4 (fun _ -> ref true)); ctype= Void})) true;
  assert_equal (is_full_cell ({walls = (Array.init 4 (fun _ -> ref false)); ctype= Void})) false

let test_convert_to_block _ = 
  let c = {walls= (Array.init 4 (fun _ -> ref false)); ctype = Void} in
  convert_to_block c;
  assert_equal (is_full_cell c && c.ctype = Block) true 

let test_is_full _ = 
  let m_full : map = {width=1;height=1;content=[|[|{walls = (Array.init 4 (fun _ -> ref true)); ctype= Void}|]|]} in
  let m_not_full : map = {width=1;height=1;content=[|[|{walls = (Array.init 4 (fun _ -> ref false)); ctype= Void}|]|]} in   
  assert_equal (is_full m_full) true;
  assert_equal (is_full m_not_full) false

let test_is_legale _ = 
  let m = (empty_map 1 1) in 
  assert_equal (is_legal m (-1,-1,N)) false;
  assert_equal (is_legal m (max_int,max_int,O)) false;
  assert_equal (is_legal m (42,42,N)) false


let test_apply_play _ =
  assert_equal (apply_play (map1_template true true true false) (0,0,E) 0) true;
  assert_equal (apply_play (map1_template true false true false) (0,0,E) 0) false

