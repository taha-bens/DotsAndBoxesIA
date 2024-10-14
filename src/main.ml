

(* Boucle principale du jeu *)
let main_loop () = 
  let continue = ref true in
  while !continue do
    Game.play_game ();
    print_string "Voulez-vous jouer à nouveau ? (o/n) : ";
    continue := match read_line () with
    | "o" | "O" | "oui" | "Oui" -> true
    | _ -> false
  done;
  print_endline "Merci d'avoir joué ! \n"


(* Début du programme *)
let () = main_loop ()