
open Map

type play = int * int * side   (* (x, y, [N-O-S-E]) *)
type game_view = Gameview (* A voir *)
type bot = game_view -> play
type player = Player of int | Bot of int * bot (* Attention le bot doit aussi avoir un id !!*)


(* Etat de la partie *)
type game_state = {score : int array; player_list : player list; next_player : player; map : map}

type error = unit (*A voir *)

(* résultat d'une action sur le jeu *)
type outcome = 
| Next of game_state
| Error of string 
| Endgame of player option

let view (_ : game_state) : game_view = Gameview

let display (_ :game_view): unit = ()


(* mise à jour du score *)
let update_score (score : int array) (id : int) = score.(id) <- score.(id) + 1

let get_player_id = function Player id -> id | Bot (id,_) -> id

let get_next_player (pl : player list) (p : player) = List.nth pl ((get_player_id p + 1) mod List.length pl)
   

(* renvoie la n-ème lettre de l'alphabet en Majuscule *)
let nth_letter n =
  if n < 1 || n > 26 then
    None (*failwith "Le nombre doit être compris entre 1 et 26"*)
  else
    Some (Char.chr (64 + n))

(* Vérifie si un coup est bien légale (bien encodé et correcte pour la map )*)
let check_move (_ : map) (_ : play) = true
  (* let alpha = match nth_letter ma.width with None -> ' '| Some c -> c in 
  let n = ma.height in  
  match mo with
  | Move (first,second,third) -> (
    if alpha = ' ' || first < 'A' || first > alpha || alpha < 'A' || alpha > 'Z' then
      Error (* failwith ("Le premier caractère doit être compris entre A et alpha = " ^ (String.make 1 alpha) ^ " (alpha < Z)") *)
    else if not (List.mem third ['N';'O';'S';'E'])then
      Error (* failwith ("Le troisième argument doit être 'N','O','S' ou bien 'E'") *)
    else if second < 1 || second > n then
      Error (* failwith (Printf.sprintf "Le deuxième caractère doit être un chiffre entre 1 et %d" n) *)
    else
      Move (first, second, third))
  | _ -> Error *)
        
let init_game_state (w: int) (h:int) (pl :player list) = 
  {score = Array.make (List.length pl) 0;
  player_list = pl; 
  next_player = List.nth pl 0; 
  map = random_map w h} 

(* Attention, ne gère pas si la hauteur est > 10 *)
let string_to_play (s : string) = (
      (Char.code s.[0]) - (Char.code 'A'),
      int_of_string (String.make 1 s.[1]),
      match s.[2] with 
      |'N' -> N 
      |'O' -> O
      |'S' -> S
      |'E' -> E
      | _ -> raise (Invalid_argument "error side")) 


let rec get_player_act () = try string_to_play (read_line ()) with _ -> print_endline "Erreur de saisie. Ressaie !"; get_player_act ()

(* vérifier que le coup est valide puis l'appliquer *)

(* Revoir les exceptions !!!!!!!!!!*)
let act (p: player) ((x,y,s) : play) (gs: game_state) : outcome = 

  try (
    if check_move gs.map (x,y,s) then (

      let id = get_player_id p in 
      let box_completed = place_wall gs.map x y s id in (* execute le coup *)
      
      if is_full gs.map then 
        Endgame (Some p) 
      else(
        
        print_endline ("le joueur : "^ string_of_int id ^ " joue !");
        if box_completed then update_score gs.score id;

        Next {
          score = gs.score;
          player_list = gs.player_list;
          next_player = get_next_player gs.player_list gs.next_player;
          map = gs.map
        })
    ) else (
      raise (Invalid_argument "le coup n'est pas valide")
    )
  ) with 
  | Invalid_argument s -> Error s


let rec game_loop outcome = 
  match outcome with
  | Next gs -> (
      print_map gs.map;
      let play = match gs.next_player with
      | Player _ -> get_player_act ()
      | Bot (_,bot) -> bot (view gs) in 
      game_loop (act gs.next_player play gs) 
    )
  | Error s -> failwith s   (* A voir ce qu'on fait ici *)
  | Endgame player -> player 
  

let play_game (w: int) (h:int) (pl :player list) = 

    match game_loop (Next (init_game_state w h pl)) with 
    | None -> print_endline "égalité"
    | Some x -> 
      match x with
      | Player _ -> print_endline "le joueur ... à gagné"
      | Bot _ -> print_endline "le bot ... à gagné";  

