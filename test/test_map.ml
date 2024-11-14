open OUnit2
open QCheck
open Dnb.Map
open Dnb.Apitype


(* map d'une seule cellule *)
let map1_template n o s e = {width=1;height=1;content=[|[|{walls= [|ref n; ref o; ref s; ref e|];ctype = Void}|]|]}
  

(* générateur d'entier entre 1 et 20 correspondants aux dimensions de la map *)
let dim_gen = QCheck.Gen.int_range 1 20

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

let test_empty_map _ =
  let m = empty_map 5 5 in
  assert_equal 5 m.width;
  assert_equal 5 m.height;
  assert_equal 5 (Array.length m.content);
  assert_equal 5 (Array.length m.content.(0))

let test_random_map _ =
  let m = random_map 10 10 in
  assert_equal 10 m.width;
  assert_equal 10 m.height;
  assert_bool "Map should contain some blocks" 
    (Array.exists (Array.exists (fun c -> c.ctype = Block)) m.content)

let test_perlin_map _ =
  let m = perlin_map 10 10 in
  assert_equal 10 m.width;
  assert_equal 10 m.height;
  assert_bool "Map should contain some blocks" 
    (Array.exists (Array.exists (fun c -> c.ctype = Block)) m.content)


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

let prop_map_dimensions =
  QCheck.Test.make ~count:100
    ~name:"Map dimensions are correct"
    (QCheck.pair (QCheck.int_range 1 100) (QCheck.int_range 1 100))
    (fun (w, h) ->
        let m = empty_map w h in
        m.width = w && m.height = h &&
        Array.length m.content = h &&
        Array.for_all (fun row -> Array.length row = w) m.content)

let prop_copy_map =
  QCheck.Test.make ~count:100
    ~name:"Copy map is identical to original"
    (QCheck.pair (QCheck.int_range 3 20) (QCheck.int_range 3 20))
    (fun (w, h) ->
        let m1 = random_map w h in
        let m2 = copy_map m1 in
        maps_equal m1 m2 cells_equal)
