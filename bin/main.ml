
(* Variable d'essais *)
let (g : Game.grid) = "grille"


(* Boucle principale du jeu *)
let rec main_loop continue = 
  if continue then
    let _ = Game.play_game Player.players g in
    print_string "score \n";
    print_string "Voulez-vous jouer à nouveau ? (o/n) : \n";
    main_loop (match read_line () with
    | "o" | "O" | "oui" | "Oui" -> true
    | _ -> false)
  else 
  print_endline "Merci d'avoir joué ! \n" 


(* Début du programme *)
let () = main_loop true