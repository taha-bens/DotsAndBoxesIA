
(* Module des futures IA, C'est ici que le programme viens chercher la liste de joueurs *)


(* stratégie d'un joueur sur le terminal*)
let strategy_terminal = fun _ ->  let s = read_line () in (s.[0],int_of_char s.[1],s.[2]) (*faire des vérifications sur l'entrée*)

(* stratégies :*)
let strategyA = fun _ -> ('F',3,'N')  

let strategyB = fun _ -> ('G',5,'O')



let make_player_list (p : int) (botslist : string list) : Game.player list = 
  List.init p (fun i -> (i + 1,strategy_terminal)) @

  List.mapi 
  (fun i s -> 
    let offset = p + i + 1 in
    match s with
    | "sA" -> (offset,strategyA)
    | "sB" -> (offset,strategyB)
    | _ -> (offset,strategy_terminal)) botslist



let (players : Game.player list) = [
  (1,fun _ -> ('F',1,'E'));
  (2,fun _ -> ('C',4,'S'));
  (3, strategyA)
  ]

