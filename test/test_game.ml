open OUnit2
open Dnb.Game
open QCheck

let test_nth_letter _ =
  assert_equal (nth_letter 1) (Some 'A');
  assert_equal (nth_letter 26) (Some 'Z');
  assert_equal (nth_letter 42) None;
  assert_equal (nth_letter (-1)) None
  

(* générateur d'entier entre 1 et 15 correspondants aux dimensions de la map *)
let dim_gen = QCheck.Gen.int_range 1 15

(* générateur de map aléatoire
  On suppose que Dnb.Map.random_map est correct *)
let random_map_gen = Gen.map2 (fun w h -> Dnb.Map.random_map w h) dim_gen dim_gen

(* générateur de map perlin
  On suppose que Dnb.Map.perlin_map est correct *)
let perlin_map_gen = Gen.map2 (fun w h -> Dnb.Map.perlin_map w h) dim_gen dim_gen


(* Prend un générateur de map et renvoie un générateur de couple (map,move) *)
let map_move_gen map_gen = 
  Gen.(map_gen >>= fun (map : Dnb.Map.map) -> 

    let c = (match Dnb.Game.nth_letter map.width with None -> ' ' | Some c -> c) in 
    let h = map.height in 

    Gen.map (fun move -> (map,move)) (Test_player.move_gen c h)
  )

(* test check_move selon différent générateur de map *)
let test_check_move map_gen = 
  let open Dnb.Player in 

  QCheck.Test.make ~count:10 ~name:"test_check_move"

  (make ~print: (fun (_,move) -> move_to_string move)
  (map_move_gen map_gen))

  (fun (map,move) -> 
    match Dnb.Game.check_move map move with
    | Error -> false
    | _ -> true
  )



(* C'est pas du tout un test valable !!! 
Ca génère une map et un joeur et ca regarde si le joueur joue bien (SUBJECTIF) !! 
Ici ça revient à tester si le coup est en dehors de la map 
A REVOIR !!
*)
let test_player_turn_play map_gen = 
  QCheck.Test.make ~count:10 ~name:"test_player_turn_play"

  (make ~print: (fun (map,(id,strat)) -> "Le joueur id = " ^ string_of_int id ^ " à voulu jouer : " ^ (Dnb.Player.move_to_string (strat map)))

  Gen.(map_gen >>= fun (map : Dnb.Map.map) -> 
    Gen.map (fun (p: Dnb.Player.player) -> (map, p))  (Test_player.player_gen map)
  ))

  (fun (map,p) -> 
    match player_turn_play map p 0 with
    | Wrong -> false
    | Ok -> true
    | BoxCompleted -> true
  
  )



  

