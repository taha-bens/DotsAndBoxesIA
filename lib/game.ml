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
| Error of (player * string)
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

let get_best_player (gs : game_state) =
	let (p : player option ref) = ref None in
	let (max : int ref) = ref (-1) in
	Array.iteri 
		(fun i s ->
			if s > !max then
				(p:=Some(Player i); max:= s)
			else if s = !max then 
				p:=None 
	) gs.score;
	!p

(* Fonctions utilitaires --------------------------------------------------- *)	  
let int_of_letters (s : string) = 
	let i = ref 0 in 
	let len = String.length s in
	String.iteri (fun j c -> 
		if c < 'A' || c > 'Z' then raise (Invalid_argument "wrong input")
		else if j = len-1 then
		  i := !i + (Char.code c - Char.code 'A') + 1
		else i := !i + (Char.code c - Char.code 'A' + 1) * (int_of_float (26.**(float_of_int (len-1-j))))
	  ) s; 
	!i - 1

let last_sequence_index (f : char -> bool) start s =
	let len = String.length s in
	let rec aux i = 
		let c = String.get s i in
		if f c then 
		if i+1<len then
			aux (i+1) else i
		else (i-1)
	in aux start 
	
let play_of_string s =
	let len = String.length s in
	let ind_1 = last_sequence_index (fun c -> 'A' <= c && c <= 'Z') 0 s in
	if ind_1 = -1 then raise (Invalid_argument "error play format1")
	else 
		let ind_2 = last_sequence_index (fun c -> '0' <= c && c <= '9') (ind_1+1) s
		in if ind_2 != len-2 then raise (Invalid_argument "error play format2")
		else 
		let s1 = String.sub s 0 (ind_1+1) in
		let s2 = String.sub s (ind_1+1) (ind_2 - ind_1) in
		let s3 = 
			match String.get s (len-1) with
			| 'N' -> N
			| 'O' -> O
			| 'S' -> S
			| 'E' -> E
			| _ -> raise (Invalid_argument "error side")
		in (int_of_letters s1, int_of_string s2, s3)

(*let play_of_string (s : string) : play = 
	let len = String.length s in
	let ind_1 = ref (-1) in
	let ind_2 = ref (-1) in
	for i = 0 to len-1 do
		let c = String.get s i in
		if 'A' <= c && c <= 'Z' then 
			if !ind_1 != i-1 then raise (Invalid_argument "error play format")
			else ind_1 := i
		else if '0' <= c && c <= '9' then 
			if !ind_2 != i-1 && !ind_2 != !ind_1 then raise (Invalid_argument "error play format")
			else ind_2 := i
		else raise (Invalid_argument "error play format")
	done;
	if !ind_1 = -1 || !ind_2 = -1 || !ind_1 >= !ind_2 || !ind_2 != (len-1) then 
		raise (Invalid_argument "error play format")
	else 
		let s1 = String.sub s 0 !ind_1 in
		let s2 = String.sub s !ind_1 (!ind_2 - !ind_1) in
		let s3 = 
			match String.get s (len-1) with
			| 'N' -> N
			| 'O' -> O
			| 'S' -> S
			| 'E' -> E
			| _ -> raise (Invalid_argument "error side")
		in (int_of_letters s1, int_of_string s2, s3)*)

let string_of_play (p : play) =
	let (i, j, s) = p in
	(letters_of_int i) ^ " " ^ (string_of_int j) ^ " " ^ (string_of_side s)

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
	with _ -> (
		print_animated "Mauvaise saisie, réessayez : "; 
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
				Endgame (get_best_player gs)
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
		Error (p, string_of_play play)

let refresh_display (gs : game_state) =
	clear_terminal ();
	print_game_state gs

let rec game_loop outcome prev_gs = 
	match outcome with
	| Next gs -> (
		let play = 
			match gs.cur_player with
			| Player _ -> 
				(print_animated ("Joueur " ^ (string_of_player gs.cur_player) ^ ", entrez un coup : ");
				get_player_play ())
			| Bot (id, bot) -> 
				(print_animated ("Le bot " ^ (string_of_int id) ^ " est en train de jouer");
				print_animatedf 0.1 "....................";
				bot (view gs))
		in let result = act gs.cur_player play gs in
		refresh_display gs;
		game_loop result gs)
	| Error (player, s) -> (
		match player with
		| Player _ -> (
			print_animated ("Vous avez entré le coup invalide " 
			^ s ^ ", réessayez\n");
			game_loop (Next(prev_gs)) prev_gs)
		| Bot _ -> (print_animated (
			"Le bot a entré le coup " 
			^ s ^ "qui est invalide, arrêt du jeu\n");
			game_loop (Endgame None) prev_gs))
	| Endgame player_opt -> player_opt

let play_game (w: int) (h: int) (pl : player list) = 
	let gs = init_game_state w h pl in
	refresh_display gs;
	match game_loop (Next gs) gs with 
	| None -> print_animated "Fin de partie, personne n'a gagné !\n"
	| Some x -> 
		match x with
		| Player id -> print_animated ("\nLe joueur "^ string_of_int id ^ " a gagné !")
		| Bot (id,_) -> print_animated ("\nLe bot "^ string_of_int id ^ " a gagné !");