open Dnb.Game
open Dnb.Apitype
open Dnb.Map
open OUnit2
open QCheck

let prop_get_player_id = 
  QCheck.Test.make ~count:10 ~name:"test_get_player_id"
  (make ~print: (fun n -> "test_get_player_id of " ^ string_of_int n)
  (QCheck.Gen.int_range 1 15))
  (fun n -> get_player_id (Player n) = n && get_player_id (Bot (n,fun _ -> (0,0,N))) = n)

let test_play_of_string _ = 
  assert_equal (play_of_string "A0N") (0,0,N);
  assert_equal (play_of_string "E4O") (4,4,O);
  assert_raises (Invalid_argument "error side") (fun () -> play_of_string "A0X");
  assert_raises (Invalid_argument "error play format1") (fun () -> play_of_string "osejfpzqejf^");
  assert_raises (Invalid_argument "error play format2") (fun () -> play_of_string "A0Nizefoie")

let test_string_of_play _ =
  assert_equal "A 0 N" (string_of_play (0, 0, N));
  assert_equal "Z 25 E" (string_of_play (25, 25, E))

let test_get_next_player _ =
  let players = [Player 0; Player 1] in
  assert_equal (Player 1) (get_next_player players (Player 0));
  assert_equal (Player 0) (get_next_player players (Player 1))

let test_get_best_player _ =
  let gs = {
    score = [|5; 10; 7|];
    player_list = [Player 0; Player 1; Player 2];
    cur_player = Player 0;
    map = empty_map 5 5
  } in
  assert_equal (Some (Player 1)) (get_best_player gs)

let test_int_of_letters _ =
  assert_equal 0 (int_of_letters "A");
  assert_equal 25 (int_of_letters "Z");
  assert_equal 26 (int_of_letters "AA");
  assert_raises (Invalid_argument "wrong input") (fun () -> int_of_letters "A1")


(* très compliqué à tester car chaque partie est unique et indépendante *)
let test_act1 _ =
  let p1, p2 = Player 0, Player 1 in 
  let pl =  [p1;p2] in
  let map = {width=1;height=1;content=[|[|{walls= [|ref false; ref false; ref true; ref true|];ctype = Void}|]|]} in 
  let gs = {score = Array.make (List.length pl) 0;
	player_list = pl; 
	cur_player = List.nth pl 0; 
	map = map} in 

  (* test l'ajout d'un mur par p1 *)
  match act (Player 0) (0,0,O) gs with 
  | Next gs ->
    assert_equal !(gs.map.content.(0).(0).walls.(1)) true;
    assert_equal gs.cur_player p2
  | Error _ -> ()
  | Endgame _ -> ();

  (* test la fin de partie avec p1 gagnant *)
  match act (Player 0) (0,0,N) gs with 
  | Next _ -> ()
  | Error _ -> ()
  | Endgame p -> match p with | None -> () | Some p -> assert_equal p p1
 
let test_act2 _ =
  let gs = {
    score = [|0; 0|];
    player_list = [Player 0; Player 1];
    cur_player = Player 0;
    map = empty_map 3 3
  } in
  match act (Player 0) (0, 0, N) gs with
  | Next new_gs -> 
    assert_equal 0 new_gs.score.(0);
    assert_equal (Player 1) new_gs.cur_player
  | _ -> assert_failure "Expected Next outcome"


let test_init_game_state _ =
  let players = [Player 0; Player 1] in
  let gs = init_game_state 5 5 players in
  assert_equal 5 gs.map.width;
  assert_equal 5 gs.map.height;
  assert_equal 2 (Array.length gs.score);
  assert_equal players gs.player_list;
  assert_equal (Player 0) gs.cur_player

let prop_get_next_player =
  QCheck.Test.make ~count:100
    ~name:"get_next_player cycles through all players"
    (QCheck.list_of_size (Gen.int_range 2 10) (QCheck.int_range 2 10))
    (fun ids ->
        let players = List.map (fun id -> Player id) ids in
        List.for_all (fun p ->
          get_next_player players p = List.nth players ((get_player_id p + 1) mod List.length players)
        ) players)

