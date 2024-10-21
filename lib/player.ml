
(* Module des futures IA, C'est ici que le programme viens chercher la liste de joueurs *)

open Map
open Display

(* move = coordonnée * mur_pos *)
type move = Move of char * int * char | Error

(* Player = id * stratégie (le player à une copie de la map) *)
type player =  int * (map -> move)

(* converti une chaîne de caractère de 3 caractères en un move *)
let string_to_move (s : string) : move =
  if String.length s <> 3 then
    Error (* failwith "La chaîne doit contenir exactement 3 caractères" *)
  else
    try (Move (s.[0], int_of_string (String.make 1 s.[1]), s.[2])) with Failure _ -> Error

let move_to_string (m : move) = 
  match m with 
  | Error -> "Error"
  | Move (x,y,z) -> (String.make 1 x) ^ (string_of_int y) ^ (String.make 1 z) 

(* stratégie d'un joueur sur le terminal*)
let strategy_terminal = fun _ -> string_to_move (read_line ())

(* stratégies :*)
let strategyA = fun _ -> Move ('F',3,'N')  

let strategyB = fun _ -> Move ('G',5,'O')


(* Crée la liste des joueurs de la partie (bots compris) *)
let rec make_player_list () : player list = 

  print_message "récup des données \nCombien de joueurs ?" ;
  let n = read_int_opt () in (*n doit être un entier*)
  let l = match n with
  | None -> (print_message "On demande un entier !\n";
            make_player_list ())
  | Some n -> (


  print_message "Quels bots ?";
    let botslist = let s = read_line () in if s = "" || s = " " then [] else String.split_on_char ',' s in   (*Gérer les cas sans bots ( '' )*)
    
    let l = 
      List.init n (fun i -> (i + 1,strategy_terminal)) @
      List.mapi 
      (fun i s -> 
        let id = n + i + 1 in
        match s with
        | "sA" -> (id,strategyA)
        | "sB" -> (id,strategyB)
        | _ -> (id,strategy_terminal)) botslist in


    if (List.length l) < 2 then (
      print_message "On ne peut pas jouer seul au jeu\n";
      make_player_list ();
    )
    else
      l

  ) in l



let (players : player list) = [
  (1,fun _ -> Move ('F',1,'E'));
  (2,fun _ -> Move ('C',4,'S'));
  (3, strategyA)
  ]

