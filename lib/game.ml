
open Player
open Map


(* La partie est elle terminée ? *)
let game_complet () = Random.bool ()


(* mise à jour du score *)
let update_score (l : int list) ((id,_) : player) = List.mapi (fun i x -> if i = id then x+1 else x) l

(* Applique la stratégie d'un jouer sur la grille, renvoie true si l'action a permis de compléter une case *)
(* Il faut vérifier que c'est bien un coup légale *)
let do_move ((id, strategie) : player) (m : map) = print_string ("Le joueur : " ^ (string_of_int id) ^ " joue " ^ ((fun (x,y,z) ->  (String.make 1 x) ^ string_of_int y ^ (String.make 1 z)) (strategie m)) ^ " sur la grille \n"); true


(* Une Partie : Prend une liste de joueurs et une grille de jeu et renvoie la liste des scores *)
let play_game (players : player list) (m : map) = 

  print_string "On lance une partie \n";


  let rec play_step score =
    if not (game_complet ()) then

      let score = List.fold_left 
        (fun score player -> 
          print_map m;
          print_string ("C'est au joueur " ^ string_of_int (fst player) ^ " de jouer : \n");
        if do_move player m then 
          update_score score player
        else score)
        score players in

    play_step score 
    else score
  in 

  play_step (List.init ((List.length players) + 1) (fun _ -> 0))
