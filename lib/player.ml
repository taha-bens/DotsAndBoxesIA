
(* Module des futures IA, C'est ici que le programme viens chercher la liste de joueurs *)

open Map

(* move = coordonnée * mur_pos *)
type move = char * int * char

(* Player = id * stratégie (le player à une copie de la map)*)
type player =  int * (map -> move)

(* converti une chaîne de caractère au bon format en un move
  alpha : borne sup des lettres 
  n : borne sup des entiers
*)
let string_to_move (alpha : char) (n : int) (s : string) : move =
  if String.length s <> 3 then
    failwith "La chaîne doit contenir exactement 3 caractères"
  else
    let first = s.[0] in
    let second = int_of_string (String.make 1 s.[1]) in
    let third = s.[2] in
    if first < 'A' || first > alpha || alpha < 'A' || alpha > 'Z' then
      failwith ("Le premier caractère doit être compris entre A et alpha = " ^ (String.make 1 alpha) ^ " (alpha < Z)")
    else if not (List.mem third ['N';'O';'S';'E'])then
      failwith ("Le troisième argument doit être 'N','O','S' ou bien 'E'")
    else if second < 1 || second > n then
      failwith (Printf.sprintf "Le deuxième caractère doit être un chiffre entre 1 et %d" n)
    else
      (first, second, third)

(* renvoie la n-ème lettre de l'alphabet en Majuscule *)
let nth_letter n =
  if n < 1 || n > 26 then
    failwith "Le nombre doit être compris entre 1 et 26"
  else
    Char.chr (64 + n)

(* stratégie d'un joueur sur le terminal*)
let strategy_terminal = fun m -> string_to_move (nth_letter m.width) m.height (read_line ())

(* stratégies :*)
let strategyA = fun _ -> ('F',3,'N')  

let strategyB = fun _ -> ('G',5,'O')


(* Crée la liste des joueurs de la partie (bots compris) *)
let make_player_list ((p : int),(botslist : string list)) : player list = 
  let l = List.init p (fun i -> (i + 1,strategy_terminal)) @

  List.mapi 
  (fun i s -> 
    let id = p + i + 1 in
    match s with
    | "sA" -> (id,strategyA)
    | "sB" -> (id,strategyB)
    | _ -> (id,strategy_terminal)) botslist in
  if (List.length l) < 2 then 
    failwith "On ne peut pas jouer seul au jeu"
  else
    l



let (players : player list) = [
  (1,fun _ -> ('F',1,'E'));
  (2,fun _ -> ('C',4,'S'));
  (3, strategyA)
  ]

