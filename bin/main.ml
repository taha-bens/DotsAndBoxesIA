open Dnb.Map
open Dnb.Game
open Dnb.Player

(* Variable d'essais *)
let (g : grid) = "grille"


(* Boucle principale du jeu *)
let rec main_loop continue = 
  if continue then
    let _ = play_game players g in
    print_string "score \n";
    print_string "Voulez-vous jouer à nouveau ? (o/n) : \n";
    main_loop (match read_line () with
    | "o" | "O" | "oui" | "Oui" -> true
    | _ -> false)
  else 
  print_endline "Merci d'avoir joué ! \n" 


(* Début du programme *)
let () = print_map (random_map 20 8);main_loop true