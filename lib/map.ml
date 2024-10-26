type side = N | O | S | E
type celltype = Void | Block | CompletedBy of int
type 'a grid = 'a array array

exception MapException of string

type cell = {walls: bool ref array; mutable ctype:celltype} (*N O S E*)

let grids_equal (arr1 : 'a grid) (arr2 : 'a grid) (f : 'a -> 'a -> bool) = 
  try
    Array.for_all2 
      (fun s_arr1 s_arr2 -> Array.for_all2 f s_arr1 s_arr2)
      arr1 arr2
  with _ -> false

let cells_equal c1 c2 = (Array.for_all2 (fun s1 s2 -> s1 = s2) c1.walls c2.walls) && (c1.ctype = c2.ctype)
                                                            
type map = {width:int; height:int; content: cell grid}
           
let get_cell i j m = m.content.(i).(j)
                       
let get_walls (c : cell) =
  c.walls 
    
let get_wall (c : cell) (s : side) =
  match s with
  | N -> c.walls.(0)
  | O -> c.walls.(1)
  | S -> c.walls.(2)
  | E -> c.walls.(3) 
           
let set_wall (c : cell) (s : side) value =
  (get_wall c s) := value
  
(*On ne peut pas utiliser la fonction get_wall ici*)
let set_wall_ref (c : cell) (s : side) bool_ref =
  match s with
  | N -> c.walls.(0) <- bool_ref
  | O -> c.walls.(1) <- bool_ref
  | S -> c.walls.(2) <- bool_ref
  | E -> c.walls.(3) <- bool_ref
  
let get_ctype (c : cell) = c.ctype
  
let set_ctype (c : cell) value = 
  c.ctype <- value

let maps_equal m1 m2 = 
  grids_equal m1.content m2.content

let is_full_cell (c : cell) =
  Array.for_all (fun (w : bool ref) -> !w) c.walls
    
let convert_to_block (c : cell) =
  Array.iter (fun w -> w := true) c.walls;
  set_ctype c Block

let fill_map (m : map) = 
  (Array.iter 
     (fun s_arr -> Array.iter 
         (fun c -> if is_full_cell c then convert_to_block c else ()) s_arr) m.content);
  m
     
let empty_cell () = {walls=Array.init 4 (fun _ -> ref false); ctype=Void}
     
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
  
let random_map w h =
  let proba_block = 0.25 in
  let m = empty_map w h in
  (Array.iter
     (fun s_arr -> Array.iter
         (fun c -> 
            if (Random.float 1.) <= proba_block then convert_to_block c else ()) s_arr) m.content);
  m
    
let buf_of_map line m = 
  let len = 6 + 7 * m.width + 1 + 1 in
  if i = -1 then
    let buf = Buffer.create len in
    (for i = 1 to len do
       if (i/7) = 0 then Buffer.add_char buf ' '
       else if i mod 7 = 0 then Buffer.add_char buf '.'
       else if i mod 7 = 3 then Buffer.add_char buf (char_of_int (i/7))
       else if i = len then Buffer.add_char buf '\n'
       else Buffer.add_char buf '-')
      buf
  else 
    let buf1 = Buffer.create len in
    let buf2 = buffer.create len in
    let buf3 = Buffer.create len in
    for i = 1 to len do
      if (i/7) = 0 then 
        (Buffer.add_char buf1 ' ';
         Buffer.add_char buf2 (if i = 3 then char_of_int (i/7) else ' ');
         Buffer.add_char buf3 ' ')
      else
		
let print_line_map m l = (*print 'l'-th line of map 'm' using the format specified in the documentation*)
if l >= m.height then () 
else
	let length = m.width * 7 + 1 in
	if l = 0 then (* first line case *)
	let bufz = Buffer.create (length) in 
	for j = 0 to length-1 do
		if j mod 7 = 0 then Buffer.add_char bufz '.' else 
		if get_bin m.content.(l).(j / 7) 0 then Buffer.add_char bufz '-' 
		else Buffer.add_char bufz ' ' 
	done;
	print_endline(Buffer.contents bufz);
	else ();
	let rec tmp b1 b2 bbot i = 
	if i >= m.width then () else
		let c = m.content.(l).(i) in 
		if i = 0 then (Buffer.add_char bbot '.';
					if get_bin c 1 then (Buffer.add_char b1 '|'; Buffer.add_char b2 '|';) 
					else (Buffer.add_char b1 ' '; Buffer.add_char b2 ' ';) )
		else (); 
		if get_bin c 2 then Buffer.add_string bbot "------" else Buffer.add_string bbot "      ";
		(match c.content with 
		| Void -> (Buffer.add_string b1 "      ";
				Buffer.add_string b2 "      ")
		| Block -> (Buffer.add_string b1 "&&&&&&";
				Buffer.add_string b2 "&&&&&&")
		| CompletedBy i -> (Buffer.add_string b1 ("!!P" ^ string_of_int i ^ "!!");
				Buffer.add_string b2 "!!!!!!"));
		if get_bin c 3 then 
		(Buffer.add_char b1 '|'; Buffer.add_char b2 '|') 
		else 
		(Buffer.add_char b1 ' '; Buffer.add_char b2 ' ');
		Buffer.add_char bbot '.';
		if i = m.width - 1 then 
		(print_endline(Buffer.contents b1); 
		print_endline(Buffer.contents b2);
		print_endline(Buffer.contents bbot))
		else tmp b1 b2 bbot (i+1)
	in tmp (Buffer.create length) (Buffer.create length) (Buffer.create length) 0
	
let print_map m = (*print map 'm' using the format specified in the documentation*)
let rec tmp i = 
	if i >= m.height then () else
	(print_line_map m i; tmp (i+1))
in tmp 0

let is_full m = (*return true if all cell content are != 0*)
let rec tmp x y = 
	if x >= m.width then tmp 0 (y+1) 
	else if y >= m.height then true
	else 
	if m.content.(y).(x).content = Void then false else tmp (x+1) y
in tmp 0 0

(*place a 'side' wall in the '(col, row)' cell and if it's full set content to 'content'
* raise Invalid_argument exception if the cell is not empty or if side is not between 0 and 3
*)
let place_wall m row col (s : side) (content : int) = 
let c = m.content.(row).(col) in
if c.content <> Void then raise (MapException "cell is not empty")
else
	(match s with
	| N -> if row = 0 then () else set_bin m.content.(row-1).(col) 2 true
	| O -> if col = 0 then () else set_bin m.content.(row).(col-1) 3 true
	| S -> if row = m.height-1 then () else set_bin m.content.(row+1).(col) 0 true
	| E -> if col = m.width-1 then () else set_bin m.content.(row).(col+1) 1 true);
set_bin c 0 (* mettre s à la place de 0 : On doit gérer le type side *) true;
if c.bin = "1111" then (c.content <- CompletedBy content; true) else false