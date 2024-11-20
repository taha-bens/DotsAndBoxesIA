open Apitype
open Utils
open Display

let get_player_id p = 
	match p with
	| Player id -> id 
	| Bot (id,_) -> id

let get_next_player (pl : player list) (p : player) = 
	List.nth pl ((get_player_id p + 1) mod List.length pl)

let rec get_player_play () : play = 
  try play_of_string (read_line ()) 
  with _ -> (print_animated "Mauvaise saisie, réessayez : "; get_player_play ())

let string_of_player (p : player) = string_of_int (get_player_id p)

let get_best_player (gs : game_state) =
  let (p : player option ref) = ref None in
  let (max : int ref) = ref (-1) in
  Array.iteri 
    (fun i s ->
      if s > !max then
        (p:=Some(Player i); max:= s)
      else if s = !max then 
        p:=None 
  ) gs.score;
  !p
  
  
