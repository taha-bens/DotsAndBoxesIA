open Map
open Display
open Apitype
open Player
open Gamestate
open Utils

(* Ce module permet de gérer le déroulement d'une partie
 * fonctions principales du module :
 * play_game : s'occupe du lancement de la partie
 * game_loop : s'occupe fonctionnement tour par tours
 * act : applique le coup spécifique d'un joueur sur l'état du jeu
*)

let view (id : int) (gs : game_state) : game_view = (id, Array.to_list gs.score, copy_map gs.map)
let update_score (score : int array) (id : int) (v : int) = score.(id) <- score.(id) + v

(* Vérifie que le coup est valide puis l'applique *)
let act (p: player) (play : play) (gs: game_state) : outcome = 
	if is_legal gs.map play then
		let id = get_player_id p in 
		let completed_cells = apply_play gs.map play id
		in if completed_cells > 0 then (
			update_score gs.score id completed_cells;
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

let rec game_loop outcome prev_gs = 
	match outcome with
	| Next gs -> (refresh_display gs;
		let play = 
			match gs.cur_player with
			| Player id -> 
				(print_animated ("Joueur " ^ (string_of_int id) ^ ", entrez un coup (ex : A0N 'case de coordonnées (A,0), murs Nord'): ");
				get_player_play ())
			| Bot (id, bot) -> 
				(print_animated ("Le bot " ^ (string_of_int id) ^ " est en train de jouer");
				print_animatedf 0.1 "....................";
				bot (view id gs))
		in let result = act gs.cur_player play gs in
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
	match let out = game_loop (Next gs) gs in refresh_display gs; out with 
	| None -> print_animated "Fin de partie, personne n'a gagné !\n"
	| Some x -> 
		match x with
		| Player id -> print_animated ("\nLe joueur "^ string_of_int id ^ " a gagné !")
		| Bot (id,_) -> print_animated ("\nLe bot "^ string_of_int id ^ " a gagné !");