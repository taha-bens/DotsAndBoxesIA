open Apitype


(* Fonctions utilitaires --------------------------------------------------- *)	  
let side_of_int i =
	match i with 
	| 0 -> N
	| 1 -> O
	| 2 -> S
	| _ -> E (* Bof, bof... *)

let int_of_side s =
	match s with
	| N -> 0
	| O -> 1
	| S -> 2
	| E -> 3

let string_of_side s =
	match s with
	| N -> "N"
	| O -> "O"
	| S -> "S"
	| E -> "E"

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

(* Convertit un entier en une chaine de caractères de la forme [A-Z]+
 * Par exemple : 0 -> "A", 25 -> "Z", 26 -> "AA", 27 -> "AB" etc...*)
let letters_of_int (i : int) =
  let rec aux i acc = 
    if i < 26 then (Char.escaped (Char.chr (i + Char.code 'A'))) :: acc
  else aux (i / 26 - 1) ((Char.escaped (Char.chr (i mod 26 + Char.code 'A'))) :: acc)
  in String.concat "" (aux i [])

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

let string_of_play (p : play) =
	let (i, j, s) = p in
	(letters_of_int i) ^ " " ^ (string_of_int j) ^ " " ^ (string_of_side s)
