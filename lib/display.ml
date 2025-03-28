
(* Module gérant l'affichage de jeu sur le terminal *)
let game_name = 
	" _____          _                               _   ____                             \n"^
	"|  __ \\        | |            /\\               | | |  _ \\                            \n"^
	"| |  | |  ___  | |_  ___     /  \\    _ __    __| | | |_) |  ___  __  __  ___  ___    \n"^
	"| |  | | / _ \\ | __|/ __|   / /\\ \\  | '_ \\  / _` | |  _ <  / _ \\ \\ \\/ / / _ \\/ __|  \n"^
	"| |__| || (_) || |_ \\__ \\  / ____ \\ | | | || (_| | | |_) || (_) | >  < |  __/\\__ \\  \n"^
	"|_____/  \\___/  \\__||___/ /_/    \\_\\|_| |_| \\__,_| |____/  \\___/ /_/\\_\ \\___||___/   \n"

let clear_terminal () = 
	if Sys.unix then
		ignore (Sys.command "clear")
	else if Sys.win32 then
		ignore (Sys.command "cls")
	else
		Printf.printf "\027[2J\027[H"

let clear_and_print message = 
	clear_terminal (); 
	print_endline message

let print_game_name () = clear_and_print game_name

let box_grid grid =
	(* Fonction auxiliaire pour trouver la largeur maximale d'une colonne *)
	let max_width_column col_index =
		List.fold_left (fun acc row ->
			if col_index < List.length row then
				max acc (String.length (List.nth row col_index))
			else acc
		) 0 grid
	in
	(* Fonction pour créer une ligne horizontale *)
	let horizontal_line widths =
		"+" ^ (List.fold_left (fun acc w -> acc ^ (String.make (w + 2) '-') ^ "+") "" widths)
	in
	(* Fonction pour créer une ligne de contenu *)
	let content_line row widths =
		"|" ^ (List.fold_left2 (fun acc s w -> 
		acc ^ " " ^ s ^ (String.make (w - String.length s + 1) ' ') ^ "|"
		) "" row widths)
	in 
	if grid = [] then "" 
	else
		let num_columns = List.fold_left (fun acc row -> max acc (List.length row)) 0 grid in
		let widths = List.init num_columns max_width_column in
		let h_line = horizontal_line widths in
		h_line ^ "\n" ^
		(String.concat ("\n" ^ h_line ^ "\n") 
			(List.map (fun row -> 
			content_line (List.map2 (fun cell width -> 
				if cell = "" then String.make width ' ' else cell
			) (row @ List.init (num_columns - List.length row) (fun _ -> "")) widths) 
			widths
			) grid)) ^
		"\n" ^ h_line

let print_animatedf (duration : float) (s : string) =
  let _ = Sys.command "stty -echo" in
	let l = String.length s in
	String.iter (
		fun c -> (
			print_char c;
			flush stdout;
			Unix.sleepf (duration /. (float_of_int l))
		)
	) s;
	let _ = Sys.command "stty echo" in ()

let print_animated = print_animatedf 0.5
	
let print_and_wait (s : string) = 
	print_animated s;
	read_line ()
