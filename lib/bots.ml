open Map
open Apitype

(* Module pour les "stratégies" des bots.
 * Chaque stratégie conçu doit être renseignée dans botList (main.ml) 
 * pour qu'elle puisse être utilisée en partie.
 *)

(* Exemple de bot stupid *)
let stupid_bot : bot =
	fun ((_,_,map) : game_view) ->
		if is_full map then 
			raise (Invalid_argument "La partie est finie")
		else 
			let p = ref (-1, -1, N) in
			let rec aux i j =
				if j >= map.width then aux (i+1) 0
				else if i >= map.height then ()
				else 
					let c = get_cell i j map in
					match get_unwalled_side c with
					| None -> aux i (j+1)
					| Some(t) -> p := (i, j, t)
			in aux 0 0;
			if !p = (-1, -1, N) then
				raise (Invalid_argument "Aucun coup possible")
			else
				!p