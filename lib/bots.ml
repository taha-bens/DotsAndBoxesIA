open Game
open Map

let stupid_bot : bot =
	fun (gv : game_view) ->
		if is_full gv then 
			raise (Invalid_argument "La partie est finie")
		else 
			let p = ref (-1, -1, N) in
			let rec aux i j =
				if j >= gv.width then aux (i+1) 0
				else if i >= gv.height then ()
				else 
					let c = get_cell i j gv in
					match get_unwalled_side c with
					| None -> aux i (j+1)
					| Some(t) -> p := (i, j, t)
			in aux 0 0;
			if !p = (-1, -1, N) then
				raise (Invalid_argument "Aucun coup possible")
			else
				!p