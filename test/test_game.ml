open OUnit2
open Dnb.Game
open Dnb.Map
open QCheck

(*let test_update_score _ =
  assert_equal (let t = [|0|] in update_score t 0; t.(0)) (1);
  assert_equal (let t = [|0;1|] in update_score t 1; t.(1)) (2);
  assert_raises (Invalid_argument "index out of bounds") (fun () -> update_score [|0|] (-1))*)

let test_get_player_id _ = 
  QCheck.Test.make ~count:10 ~name:"test_get_player_id"
  (make ~print: (fun n -> "test_get_player_id of " ^ string_of_int n)
  (QCheck.Gen.int_range 1 15))
  (fun n -> get_player_id (Player n) = n && get_player_id (Bot (n,fun _ -> (0,0,N))) = n)

let test_play_of_string _ = 
  assert_equal (play_of_string "A0N") (0,0,N);
  assert_equal (play_of_string "E4O") (4,4,O);
  assert_equal (play_of_string "A0Nizefoie") (0,0,N);
  assert_raises (Invalid_argument "error side") (fun () -> play_of_string "osejfpzqejf^")


(* très compliqué à tester car chaque partie est unique et indépendante *)
let test_act _ =
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
 

  

