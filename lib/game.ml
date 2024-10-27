open Map
open Display


(* Types et exceptions ----------------------------------------------------- *)
type game_view = Gameview (* A voir *)
type bot = game_view -> play
type player = Player of int | Bot of int * bot
type game_state = {
	score : int array; 
	player_list : player list; 
	next_player : player; 
	map : Map.map }
type outcome = 
| Next of game_state
| Error of string 
| Endgame of player option
let view (_ : game_state) : game_view = Gameview
let display (_ :game_view): unit = ()


(* Getters/Setters --------------------------------------------------------- *)
let update_score (score : int array) (id : int) = 
	score.(id) <- score.(id) + 1

let get_player_id p = 
	match p with
	| Player id -> id 
	| Bot (id,_) -> id

let get_next_player (pl : player list) (p : player) = 
	List.nth pl ((get_player_id p + 1) mod List.length pl)


(* Fonctions utilitaires --------------------------------------------------- *)
let nth_letter n =
	if n < 1 || n > 26 then
		None (*failwith "Le nombre doit être compris entre 1 et 26"*)
	else
		Some (Char.chr (64 + n))

(* Attention, ne gère pas si la hauteur est > 10 *)
let play_of_string (s : string) : play = (
	(Char.code s.[0]) - (Char.code 'A'),
	int_of_string (String.make 1 s.[1]),
	match s.[2] with 
	|'N' -> N 
	|'O' -> O
	|'S' -> S
	|'E' -> E
	| _ -> raise (Invalid_argument "error side"))

let string_of_player (p : player) = 
	match p with
	| Player id -> "Joueur : " ^ string_of_int id
	| Bot (id, _) -> "Bot : " ^ string_of_int id

let print_game_state (gs : game_state) = 
	let pl = gs.player_list in 
	let grid = [ 
		"Type"::(List.init (List.length pl) (fun i -> (match List.nth pl i with | Player _ -> "Player" | Bot _ -> "Bot")));
		"Id"::(List.init (List.length pl) (fun i -> string_of_int (get_player_id (List.nth pl i))));
		"Score":: (List.init (List.length pl) (fun i -> string_of_int gs.score.(i)));
		"Is playing":: (List.init (List.length pl) (fun i -> if gs.next_player = (List.nth pl i) then "X" else ""));
	] in
	clear_and_print (box_grid grid) (*Mettre l'affichage de la map ici *)


(* Fonctions relatives au déroulement de la partie ------------------------- *)
let init_game_state (w: int) (h:int) (pl :player list) = 
	{score = Array.make (List.length pl) 0;
	player_list = pl; 
	next_player = List.nth pl 0; 
	map = random_map w h} 

let rec get_player_act () : play = 
	try play_of_string (read_line ()) 
	with _ -> 
		print_endline (box_string "Erreur de saisie. Ressaie !"); 
		get_player_act ()


(* Vérifie que le coup est valide puis l'applique *)
let act (p: player) ((x,y,s) : play) (gs: game_state) : outcome = 
	if is_legal gs.map (x, y, s) then
		let id = get_player_id p in 
		let box_completed = apply_play gs.map (x, y, s) id in (* execute le coup *)
		if is_full gs.map then 
			Endgame (Some p) 
		else (
			if box_completed then update_score gs.score id;
			Next {
				score = gs.score;
				player_list = gs.player_list;
				next_player = get_next_player gs.player_list gs.next_player;
				map = gs.map
			}
		)
	else
		match p with
		| Player _ -> print_mess "Vous vous êtes trompé"; Next gs   (* Le joueur à le droit de se tromper *)
		| Bot _-> Error "Un bot n'a pas le droit de se tromper" (* Le bot ne rejoue pas pour éviter les récursions infinies *)

let rec game_loop outcome = 
	match outcome with
	| Next gs -> (
			print_game_state gs;
			print_endline(Buffer.contents (buf_of_map gs.map));
			let play = match gs.next_player with
			| Player _ -> get_player_act ()
			| Bot (_, bot) -> bot (view gs) in 
			print_mess ("Le joueur " ^ (string_of_player gs.next_player) ^ " joue"); 
			game_loop (act gs.next_player play gs)
		)
	| Error s -> print_mess s; None
	| Endgame player -> player 

let play_game (w: int) (h: int) (pl : player list) = 
	match game_loop (Next (init_game_state w h pl)) with 
	| None -> print_mess "Égalité !"
	| Some x -> 
	match x with
	| Player id -> print_mess ("Le joueur "^ string_of_int id ^" a gagné !")
	| Bot (id,_) -> print_mess ("Le bot "^ string_of_int id ^ " a gagné !");  