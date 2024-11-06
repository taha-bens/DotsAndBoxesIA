open Map
open Display


(* Ce module permet de gérer le déroulement d'une partie
 * fonctions principales du module :
 * play_game : s'occupe du lancement de la partie
 * game_loop : s'occupe fonctionnement tour par tours
 * act : applique le coup spécifique d'un joueur sur l'état du jeu
*)

(* Types et exceptions ----------------------------------------------------- *)
type game_view = map
type bot = game_view -> play
type player = 
| Player of int 
| Bot of int * bot
type game_state = {
	score : int array; 
	player_list : player list; 
	cur_player : player; 
	map : map }
type outcome = 
| Next of game_state
| Error of string 
| Endgame of player option
let view (gs : game_state) : game_view = copy_map gs.map
let display (gv : game_view): unit = print_endline (Buffer.contents (buf_of_map gv))


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

let print_play (p : play) =
	let (i, j, s) = p in
	print_endline ("(" ^ (string_of_int i) ^", " ^ (string_of_int j) ^ ", " ^ (string_of_int (int_of_side s)) ^ ")")

let get_player_id (p : player) = 
	match p with
	| Player(k) -> k
	| Bot(k, _) -> k

let string_of_player (p : player) =
	string_of_int (get_player_id p)

let print_game_state (gs : game_state) = 
	let pl = gs.player_list in 
	let grid = [ 
		"Type"::(List.init (List.length pl) (fun i -> (match List.nth pl i with | Player _ -> "Player" | Bot _ -> "Bot")));
		"Id"::(List.init (List.length pl) (fun i -> string_of_int (get_player_id (List.nth pl i))));
		"Score":: (List.init (List.length pl) (fun i -> string_of_int gs.score.(i)));
		"Is playing":: (List.init (List.length pl) (fun i -> if (get_player_id gs.cur_player) = i then "X" else ""));
	] in 
	print_endline (box_grid grid);
	print_endline "";
	print_endline(Buffer.contents (buf_of_map gs.map))


(* Fonctions relatives au déroulement de la partie ------------------------- *)
let init_game_state (w: int) (h:int) (pl :player list) = 
	{score = Array.make (List.length pl) 0;
	player_list = pl; 
	cur_player = List.nth pl 0; 
	map = random_map w h} 

let rec get_player_play () : play = 
	try play_of_string (read_line ()) 
	with _ -> 
		(print_endline (box_string "Erreur de saisie. Ressaie !"); 
		get_player_play ())


(* Vérifie que le coup est valide puis l'applique *)
let act (p: player) (play : play) (gs: game_state) : outcome = 
	if is_legal gs.map play then
		let id = get_player_id p in 
		(* TODO : gérer le cas où deux cases sont complétées en même temps, éventuellement déplacer apply_play dans game *)
		let box_completed = apply_play gs.map play id in
		if box_completed then (
			update_score gs.score id;
			if is_full gs.map then
				Endgame(Some p)
			else
				Next {
					score = gs.score;
					player_list = gs.player_list;
					cur_player = gs.cur_player;
					map = gs.map
				})
		else 
			Next {
				score = gs.score;
				player_list = gs.player_list;
				cur_player = get_next_player gs.player_list gs.cur_player;
				map = gs.map
			}
	else
		match p with
		| Player _ -> print_endline "Vous vous êtes trompé"; Next gs   (* Le joueur à le droit de se tromper *)
		| Bot _-> Error "Un bot n'a pas le droit de se tromper" (* Le bot ne rejoue pas pour éviter les récursions infinies *)

let rec game_loop outcome = 
	match outcome with
	| Next gs -> (
			clear_terminal ();
			print_game_state gs; 
			let play = 
				match gs.cur_player with
				| Player _ -> 
					(print_animated ("Joueur " ^ (string_of_player gs.cur_player) ^ ", entrez un coup : ");
					get_player_play ())
					(*(print_string ("Joueur " ^ (string_of_player gs.cur_player) ^ ", entrez un coup : ");
					get_player_play ())*)
				| Bot (id, b) -> 
					(print_animated ("Le bot " ^ (string_of_int id) ^ " est en train de jouer");
					print_animatedf 1.5 "....................";
					b (view gs))
			in game_loop (act gs.cur_player play gs)
		)
	| Error s -> (print_mess s; None)
	| Endgame player_opt -> player_opt

let play_game (w: int) (h: int) (pl : player list) = 
	match game_loop (Next (init_game_state w h pl)) with 
	| None -> print_animated "Égalité !\n"
	| Some x -> 
		match x with
		| Player id -> print_animated ("\nLe joueur "^ string_of_int id ^ " a gagné !")
		| Bot (id,_) -> print_animated ("\nLe bot "^ string_of_int id ^ " a gagné !");