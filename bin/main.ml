
(*open Dnb.Map*)
open Dnb.Game
(*open Dnb.Player*)

(* prend un nombre de joueurs "terminal" et une liste de bots et renvoie la liste des joueur pour la partie *)
let make_player_list (nb_players : int) (botslist : bot list) : player list = 
  List.init nb_players (fun i -> Player i) @ List.mapi (fun i b -> Bot (nb_players + i + 1,b)) botslist


let rec get_int (mess : string) (mess_error : string) = 
  print_endline mess; 
  match read_int_opt () with
  | None -> get_int mess_error mess_error  
  | Some n -> n

(* Boucle principale du jeu *)
let rec main_loop continue =
  if continue then (

    let mess_error = "mauvaise saisie ! Réessaie !" in 
    let w = get_int "largeur de la map : " mess_error in 
    let h = get_int "hauteur de la map : " mess_error in
    let nb_p = get_int "nombre de joueurs : " mess_error in 

    print_endline "On lance la partie ";

    play_game w h (make_player_list nb_p []);

    print_endline "Voulez-vous jouer à nouveau ? (o/n) :";

    main_loop (match read_line () with
    | "o" | "O" | "oui" | "Oui" -> true
    | _ -> false)
  )
  else 
  print_endline "Merci d'avoir joué ! \n" 


(* Début du programme *)
let () = main_loop true