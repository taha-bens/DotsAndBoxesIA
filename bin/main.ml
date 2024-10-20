
open Dnb.Map
open Dnb.Game
open Dnb.Player


let get_infos () = 
  print_string "récup des données \nCombien de joueurs ?" ;
  let n = read_int () in (*n doit être un entier*)
  print_string "Quels bots ?";
  let bots = let s = read_line () in if s = "" then [] else String.split_on_char ' ' s in   (*Gérer les cas sans bots ( '' )*)
  (n,bots)


(* Boucle principale du jeu *)
let rec main_loop continue m = 
  if continue then
 

    let players = make_player_list (get_infos ()) in 
    print_string (List.fold_left (fun s p -> s ^ (string_of_int (fst p))) "" players);
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