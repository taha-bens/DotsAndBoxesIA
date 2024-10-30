open Dnb.Game
open Dnb.Display

let (botList : bot list) = []
let get_bot_by_id = List.nth botList

let rec get_input_dimensions () =
	print_string "Choisissez les dimensions du plateau \"largeur,hauteur\" : ";
	let input = read_line () in
	let dims = String.split_on_char ',' input in
	if List.length dims <> 2 then
		(print_endline "\nMauvaise saisie, réessayez";
		get_input_dimensions ())
	else
		try 
			let w = int_of_string (List.nth dims 0) in
			let h = int_of_string (List.nth dims 1) in
			if w < 2 || h < 2 then (
				print_endline "Les dimensions doivent être supérieures à 1";
				get_input_dimensions ()
			)
			else (w, h)
		with _ ->
			(print_endline "\nMauvaise saisie, réessayez";
			get_input_dimensions ())

let rec get_player_bot_numbers () = 
	print_string "Choisissez le nombre de joueurs et de bots \"joueurs, bots, idBot1, ..., idBotN\" : ";
	let input = read_line () in
	try 
		let numbers = List.map int_of_string (String.split_on_char ',' input) in
		if List.for_all (fun v -> v >= 0) numbers && (List.length numbers) = 2 + (List.nth numbers 1) then
			numbers
		else 
			(print_endline "\nMauvaise saisie, réessayez";
			get_player_bot_numbers ())
	with _ ->
		(print_endline "\nMauvaise saisie, réessayez";
		get_player_bot_numbers ())
	

(* Boucle principale du jeu *)
let rec main_loop continue =
	if continue then (
		clear_and_print game_name;
		prerr_endline "'Entrer' pour lancer une partie";
		let _ = read_line () in
		clear_terminal ();

		let (w, h) = get_input_dimensions () in
		clear_terminal ();
		let argv = get_player_bot_numbers () in
		clear_terminal ();
		let nb_p, nb_b = List.nth argv 0, List.nth argv 1 in
		let player_list = List.init (nb_p + nb_b) (fun i -> if i < nb_p then Player i else Bot(i, get_bot_by_id (List.nth argv (2 + i)))) in
		play_game w h player_list;

		print_mess "Voulez-vous jouer à nouveau ? (o/n) :";
		main_loop (match read_line () with
		| "o" | "O" | "oui" | "Oui" -> true
		| _ -> false)
	)
	else print_mess "Merci d'avoir joué ! \n" 

(* Début du programme *)
let () = main_loop true