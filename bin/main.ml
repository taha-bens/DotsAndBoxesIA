
(*open Dnb.Map*)
open Dnb.Game
(*open Dnb.Player*)

(* Boucle principale du jeu *)
let rec main_loop continue =
  if continue then (

    print_endline "On lance la partie ";

    play_game 20 8 [Player 0; Player 1];

    print_endline "Voulez-vous jouer à nouveau ? (o/n) :";

    main_loop (match read_line () with
    | "o" | "O" | "oui" | "Oui" -> true
    | _ -> false)
  )
  else 
  print_endline "Merci d'avoir joué ! \n" 


(* Début du programme *)
let () = main_loop true