(* Types et exceptions ----------------------------------------------------- *)
(* Les maps sont représentées par tableaux 2D de cellules composées de quatre
* murs et d'un contenu. Les cases qui sont interdites sont les Blocks, les  
* cases vides sont les Void et les cases qui ont été gagnée par un joueur   
* sont les CompletedBy('id du joueur'). Pour s'assurer de la cohérence de la
* map (c'est-à-dire s'assuer de la présence ou non d'un même mur sur deux  
* cases adjacentes), les murs sont représentés par des ref bool, et deux    
* cases adjacentes partagent un même mur. *)
type side = N | O | S | E
type celltype = Void | Block | CompletedBy of int
type 'a grid = 'a array array
type cell = {walls: bool ref array; mutable ctype:celltype} (*N O S E*)
type map = {width:int; height:int; content: cell grid}
exception MapException of string


(* Fonctions d'égalités pour les types qu'on a défini ---------------------- *)
let grids_equal (arr1 : 'a grid) (arr2 : 'a grid) (f : 'a -> 'a -> bool) = 
	try
	Array.for_all2 
		(fun s_arr1 s_arr2 -> Array.for_all2 f s_arr1 s_arr2)
		arr1 arr2
	with _ -> false

let cells_equal c1 c2 = 
	(Array.for_all2 (fun s1 s2 -> s1 = s2) c1.walls c2.walls) && (c1.ctype = c2.ctype)

let maps_equal m1 m2 = 
	grids_equal m1.content m2.content
															

(* Getters/Setters --------------------------------------------------------- *)
let get_cell i j m = m.content.(i).(j)
					
let get_walls (c : cell) = c.walls 
	
(* Renvoie la référence du mur *)
let get_wall (c : cell) (s : side) =
	match s with
	| N -> c.walls.(0)
	| O -> c.walls.(1)
	| S -> c.walls.(2)
	| E -> c.walls.(3) 
		
(* Renvoie la valeur du mur *)
let get_wall_val (c : cell) (s : side) = !(get_wall c s)
		
let set_wall (c : cell) (s : side) value = (get_wall c s) := value

(* On ne peut pas utiliser la fonction get_wall ici
* car le résultat de get_wall est une valeur et pas
* une variable modifiable. *)
let set_wall_ref (c : cell) (s : side) bool_ref =
	match s with
	| N -> c.walls.(0) <- bool_ref
	| O -> c.walls.(1) <- bool_ref
	| S -> c.walls.(2) <- bool_ref
	| E -> c.walls.(3) <- bool_ref

let get_ctype (c : cell) = c.ctype

let set_ctype (c : cell) value = 
	c.ctype <- value


(* Fonctions utilitaires --------------------------------------------------- *)
let is_full_cell (c : cell) =
	Array.for_all (fun (w : bool ref) -> !w) c.walls
	
let convert_to_block (c : cell) =
	Array.iter (fun w -> w := true) c.walls;
	set_ctype c Block
	
let empty_cell () = {walls=Array.init 4 (fun _ -> ref false); ctype=Void}
	
(* Renvoie un array contenant les voisins nords et 
* ouest s'ils existent, et une cellule vide (poubelle)
* sinon *)
let get_NO_neighbors i j m =
	let (l : cell list) = [] in 
	let l = 
		if i > 0 then (get_cell (i-1) j m)::l else (empty_cell ())::l in
	let l = 
		if j > 0 then (get_cell i (j-1) m)::l else (empty_cell ())::l in
	Array.of_list l

let empty_map w h =
	let return = Array.init h (fun _ -> Array.init w (fun _ -> empty_cell ())) in
	let m = {width = w; height = h; content = return} in
	Array.iteri 
		(fun i s_arr -> Array.iteri
			(fun j c ->
			let n = get_NO_neighbors i j m in
			set_wall_ref c N (get_wall n.(1) S);
			set_wall_ref c O (get_wall n.(0) E)) s_arr) m.content;
	m

(* Permet de transformer en Blocks les cases qui seraient malencontreusement 
* fermées à la suite de la génération aléatoire de Blocks *)
let fill_map (m : map) = 
	(Array.iter 
		(fun s_arr -> Array.iter 
			(fun c -> 
				if is_full_cell c then convert_to_block c 
				else ()
			) 
		s_arr) 
	m.content);
	m


(* Fonctions de génération de map ------------------------------------------ *)
let random_map w h =
	let proba_block = 0.25 in
	let m = empty_map w h in
	(Array.iter
		(fun s_arr -> Array.iter
			(fun c -> 
				if (Random.float 1.) <= proba_block then convert_to_block c 
				else ()) s_arr) m.content);
	fill_map m 

let perlin_map w h =
	let m = empty_map w h in
	let perlin = perlin_noise_grid_bool w h 10. in
	Array.iteri
		(fun i s_arr -> Array.iteri
			(fun j c ->
				if perlin.(i).(j) then convert_to_block c
				else ()
			) s_arr
		) m.content;
	fill_map m


(* Fonctions d'affichage --------------------------------------------------- *)  
let buf_of_line line (m : map) = 
	let len = 6 + 7 * m.width + 1 + 1 in
	if line = -1 then
		let buf = Buffer.create len in
			for i = 0 to m.width do 
				(if i = 0 then Buffer.add_string buf "      "
				else if i < 10 then Buffer.add_string buf ("   " ^ (string_of_int i) ^ "   ")
				else Buffer.add_string buf ("  " ^ (string_of_int i) ^ "   "))
			done;
		Buffer.add_char buf '\n';
		buf
	else 
		let buf1 = Buffer.create len in
		let buf2 = Buffer.create len in
		let buf3 = Buffer.create len in

		(Buffer.add_string buf1 "      ";
		(if line < 10 then Buffer.add_string buf2 ("   " ^ (string_of_int line) ^ "  ")
		else Buffer.add_string buf2 ("  " ^ (string_of_int line) ^ "  "));
		Buffer.add_string buf3 "      ");

		(for i = 0 to (m.width-1) do
			let c = get_cell line i m in

			(if i = 0 then 
				let ch = if get_wall_val c O then '|' else ' ' in
				(Buffer.add_char buf1 ch;
				Buffer.add_char buf2 ch;
				Buffer.add_char buf3 '.'));

			(match c.ctype with
				| Void -> 
					(Buffer.add_string buf1 "      ";
					Buffer.add_string buf2 "      ")
				| Block ->
					(Buffer.add_string buf1 "&&&&&&";
					Buffer.add_string buf2 "&&&&&&")
				| CompletedBy(k) ->
					(Buffer.add_string buf1 ("!!P" ^ string_of_int k ^ "!!");
					Buffer.add_string buf2 "!!!!!!"));

			(if get_wall_val c S then
				Buffer.add_string buf3 "------"
				else Buffer.add_string buf3 "      ");

			(let ch = if get_wall_val c E then '|' else ' ' in
				(Buffer.add_char buf1 ch;
				Buffer.add_char buf2 ch;
				Buffer.add_char buf3 '.'))
		done;);

		Buffer.add_char buf1 '\n';
		Buffer.add_buffer buf1 buf2;
		Buffer.add_char buf1 '\n';
		Buffer.add_buffer buf1 buf3;
		Buffer.add_char buf1 '\n';

		if line = 0 then
			let buf4 = Buffer.create len in
			Buffer.add_string buf4 "      ";

			(for j = 0 to (m.width-1) do
				(if j = 0 then Buffer.add_char buf4 '.');
				(if get_wall_val (get_cell 0 j m) N then Buffer.add_string buf4 "------"
				else Buffer.add_string buf4 "      ");
				Buffer.add_char buf4 '.';
			done;);
			
			Buffer.add_char buf4 '\n';
			Buffer.add_buffer buf4 buf1;
			buf4

		else buf1
	
let buf_of_map (m : map) =
	(* dégueu *)
	let buf = Buffer.create ((3 * m.height + 1) * (6 + 7 * m.width + 1 + 1)) in
	for i = -1 to m.height-1 do
		Buffer.add_buffer buf (buf_of_line i m);
	done;
	buf

(* Fonctions de manipulation ----------------------------------------------- *)
let is_legal (m : map) (p : Game.play) =
	let (i, j, s) = p in
	if i < 0 || i >= m.height || j < 0 || j >= m.width then false
	else
		not (get_wall_val (get_cell i j m) s)

let apply_play (m : map) (p : Game.play) =
	let (i, j, s) = p in
	set_wall (get_cell i j m) s true