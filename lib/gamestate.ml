open Apitype
open Player 
open Display
open Map

let init_game_state (w: int) (h:int) (pl :player list) = 
	{score = Array.make (List.length pl) 0;
	player_list = pl; 
	cur_player = List.nth pl 0; 
	map = if Random.bool () then perlin_map w h else random_map w h} 

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

let refresh_display (gs : game_state) = clear_terminal (); print_game_state gs
