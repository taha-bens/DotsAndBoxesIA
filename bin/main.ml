open Dnb.Game
open Dnb.Display
open Dnb.Bots

(* Module pricipal du programme qui gère les premières interactions
 * et le lancement de la partie.
 * Une partie peut se faire entre joueurs, entre joueurs et bots, et entre bots
 * Chaque "stratégie" de bot est stockée dans la liste botList, 
 * lui attribuant un id en fonction de l'indice dans cette dernière.
*)

let (botList : bot list) = [stupid_bot]
let get_bot_by_id = List.nth botList

let rec ask_user_dimensions (printed : bool) =
	(if not printed then print_animated "Choisissez les dimensions du plateau \"largeur,hauteur\" : ");
	let input = read_line () in
	let dims = String.split_on_char ',' input in
	if List.length dims <> 2 then
		(print_animated "Mauvaise saisie, réessayez : ";
		ask_user_dimensions true)
	else
		try 
			let w = int_of_string (List.nth dims 0) in
			let h = int_of_string (List.nth dims 1) in
			if w < 2 || h < 2 then (
				print_animated "Les dimensions doivent être supérieures à 1, réessayez : ";
				ask_user_dimensions true
			)
			else (w, h)
		with _ ->
			(print_animated "Mauvaise saisie, réessayez : ";
			ask_user_dimensions true)

let rec ask_user_nb_players (printed : bool) = 
	(if not printed then 
		print_animated "Choisissez le nombre de joueurs et de bots \"joueurs,bots,idBot1,...,idBotN\" : ");
	let input = read_line () in
	try 
		let numbers = List.map int_of_string (String.split_on_char ',' input) in
		if List.for_all (fun v -> v >= 0) numbers && (List.length numbers) = 2 + (List.nth numbers 1) then
			numbers
		else 
			(print_animated "Mauvaise saisie, réessayez : ";
			ask_user_nb_players true)
	with _ ->
		(print_animated "Mauvaise saisie, réessayez : ";
		ask_user_nb_players true)
	

(* Boucle principale du jeu *)
let rec main_loop continue =
	if continue then (
		clear_and_print game_name;
		let _ = print_and_wait "Appuyez sur 'Entrer' pour lancer la partie : " in
		clear_terminal ();

		let (w, h) = ask_user_dimensions false in
		clear_terminal ();
		let argv = ask_user_nb_players false in
		clear_terminal ();
		let (nb_p, nb_b) = (List.nth argv 0, List.nth argv 1) in
		let player_list = List.init (nb_p + nb_b) (fun i -> if i < nb_p then Player i else Bot(i, get_bot_by_id (List.nth argv (2 + i - nb_p)))) in
		play_game w h player_list;

		print_animated "Voulez-vous jouer à nouveau ? (o/n) :";
		main_loop (match read_line () with
		| "o" | "O" | "oui" | "Oui" -> true
		| _ -> false)
	)
	else print_animated "\nMerci d'avoir joué ! \n" 

let () = main_loop true