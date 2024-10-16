
(* type (provisoire ?) *)
type content =  Block |Content of string 

type grid = string (*Array of array of content *)

(* move = coordonnée * content *)
type move = string * content

(* Player = id * stratégie *)
type player =  int * (grid -> move)

(* La partie est elle terminée ? *)
let game_complet () = Random.bool ()

(* modification de la grille *)
let make_wall () = ""

(* mise à jour du score *)
let update_score (l : int list) ((id,_) : player) = List.mapi (fun i x -> if i = id then x+1 else x) l

(* Applique la stratégie d'un jouer sur la grille, renvoie true si l'action a permis de compléter une case *)
(* Il faut vérifier que c'est bien un coup légale *)
let do_move ((id, strategie) : player) (g : grid) = print_string ("Le joueur : " ^ (string_of_int id) ^ " joue " ^ fst (strategie g) ^" sur la grille \n"); true


(* Une Partie : Prend une liste de joueurs et une grille de jeu et renvoie la liste des scores *)
let play_game (players : player list) (g : grid) = 

  let rec play_step score =
    if not (game_complet ()) then

      let score = List.fold_left 
        (fun score player -> 
        if do_move player g then 
          update_score score player
        else score)
        score players in

    play_step score 
    else score
  in 

  play_step []
