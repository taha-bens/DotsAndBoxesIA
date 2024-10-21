
open Player
open Map


type move_result = Wrong | Ok | BoxCompleted

(* La partie est elle terminée ? *)
let game_complet () = Random.bool ()

(* mise à jour du score *)
let update_score (l : int list) (id : int) = List.mapi (fun i x -> if i = id then x+1 else x) l

(* Applique le move d'un jouer sur la grille, renvoie true si l'action a permis de compléter une case *)
let do_move (_ : map) (_ : move) = true

(* renvoie la n-ème lettre de l'alphabet en Majuscule *)
let nth_letter n =
  if n < 1 || n > 26 then
    failwith "Le nombre doit être compris entre 1 et 26"
  else
    Char.chr (64 + n)

(* Vérifie si un coup est bien légale *)
let check_move (ma : map) (mo : move) = 
  let alpha = nth_letter ma.width in 
  let n = ma.height in  
  match mo with
  | Move (first,second,third) -> (
    if first < 'A' || first > alpha || alpha < 'A' || alpha > 'Z' then
      Error (* failwith ("Le premier caractère doit être compris entre A et alpha = " ^ (String.make 1 alpha) ^ " (alpha < Z)") *)
    else if not (List.mem third ['N';'O';'S';'E'])then
      Error (* failwith ("Le troisième argument doit être 'N','O','S' ou bien 'E'") *)
    else if second < 1 || second > n then
      Error (* failwith (Printf.sprintf "Le deuxième caractère doit être un chiffre entre 1 et %d" n) *)
    else
      Move (first, second, third))
  | _ -> Error
    

(* Effectue le tour d'un joueur, renvoie le resultat du tour 
- Wrong : le joueur n'a pas su ou pu jouer au bout de ses 5 tentatives (cas de base pour qu'un bot ne boucle pas pendant son tour)
- Ok : le coup est valide 
- BoxCompleted : le joueur à complété une case *)
let rec player_turn_play (m : map) ((id,strategy) : player) (attempt : int) =
  if attempt >= 5 then Wrong 
  else
    (
    print_string ("C'est au joueur " ^ string_of_int id ^ " de jouer : \n");
    print_map m;
    let mv = (match check_move m (strategy m) with
    | Error -> print_string "Le coup n'est pas valide, Réessaie !\n" ;player_turn_play m (id,strategy) (attempt +1)
    | Move (x,y,z) ->  if do_move m (Move (x,y,z)) then BoxCompleted else Ok) in 
    mv
    ) 
    
  

(* Une Partie : Prend une liste de joueurs et une grille de jeu et renvoie la liste des scores *)
let play_game (players : player list) (m : map) = 

  print_string "On lance une partie \n";


  let rec play_step score =
    if not (game_complet ()) then

      let score = List.fold_left 
        (fun score player -> 
          match player_turn_play m player 0 with
          | Wrong -> print_string "Le joueur n'a pas pu jouer \n"; score;
          | Ok -> score;
          | BoxCompleted -> update_score score (fst player)
        )
        score players in

    play_step score 
    else score
  in 

  play_step (List.init ((List.length players) + 1) (fun _ -> 0))
