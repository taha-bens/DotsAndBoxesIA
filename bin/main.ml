
open Dnb.Map
open Dnb.Game
open Dnb.Player




(* Boucle principale du jeu *)
let rec main_loop continue m = 
  if continue then
 

    let players = (make_player_list ()) in 
    let score = play_game players m in
    print_string ("score : " ^ (String.concat ", " (List.map string_of_int (List.tl score))) ^ "\n");

    print_string "Voulez-vous jouer à nouveau ? (o/n) : \n";

    main_loop (match read_line () with
    | "o" | "O" | "oui" | "Oui" -> true
    | _ -> false) m
  else 
  print_endline "Merci d'avoir joué ! \n" 


(* Début du programme *)
let () = let m = random_map 20 8 in main_loop true m