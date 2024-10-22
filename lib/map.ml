Random.init;;

type cell = {mutable bin:string; mutable content:int} (*Norme binaire : NOSE*)
            
type map = {width:int; height:int; content: cell array array} (* width and height need to be >= 2 *)
           
(*'read formatted binary' - format : "0100", 0 <= i <= 3
 * raise Invalid_argument if 's' doesn't respect the specified format
*)
let rfb (s : string) (i : int) =
  let c = String.get s i in 
  match c with
  | '0' -> false
  | '1' -> true
  | _ -> raise (Invalid_argument "wrong string format")
           
let modify_string s i c = (* strings are not mutable in OCaml... :-(  *)
  let arr = Array.of_list (List.init (String.length s) (String.get s)) in
  arr.(i) <- c;
  String.init (Array.length arr) (Array.get arr)
           
(*'write formatted binary' - format : "0100", 0 <= i <= 3)
 * raise Invalid_argument if 'i' is not between 0 and 3
*)
let wfb (s : string) (i : int) (v : bool) = 
  let c = if v then '1' else '0' in
  if i >= 0 && i < 4 then modify_string s i c else 
    raise (Invalid_argument "index out of bounds : required 0 <= i < 4")
      
let set_bin (c : cell) (i : int) (v : bool) = (*set cell 'c' binary side 'i' to 'v'*)
  c.bin <- wfb c.bin i v
      
let get_bin (c : cell) (i : int) = (*get cell 'c' binary side 'i'*)
  rfb c.bin i

let fill_map (g : cell array array) = (*set "1111" cells content to -1*)
  let height = Array.length g in
  let width = Array.length g.(0) in
  for i = 0 to height-1 do
    for j = 0 to width-1 do
      if g.(i).(j).bin = "1111" then g.(i).(j).content <- -1
    done
  done;
  g
  
let random_map w h = (*random map generation with ~25% blocks*)
  let return : cell array array = Array.init h (fun _ -> Array.init w (fun _ -> {bin="0000"; content= 0})) in
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      if Random.bool () && Random.bool () then 
        (return.(i).(j) <- {bin="1111"; content= -1};
         if j > 0 then set_bin return.(i).(j-1) 3 true else ();
         if j < w-1 then set_bin return.(i).(j+1) 1 true else ();
         if i > 0 then set_bin return.(i-1).(j) 2 true else ();
         if i < h-1 then set_bin return.(i+1).(j) 0 true else ())
      else ()
    done
  done;
  {width=w; height=h; content=fill_map return}

    (*let perlin_map w h = (*procedural map generation using perlin noise defined in 'perlin.ml'*)
       let return = Array.map (fun arr -> Array.map (fun b -> if b then {bin="1111"; content= -1} else {bin="0000"; content=0}) arr) (Perlin.perlin_noise_grid_bool w h 2.) in
       for i = 0 to h-1 do
         for j = 0 to w-1 do
           if return.(i).(j).content = -1 then 
             (if j > 0 then set_bin return.(i).(j-1) 3 true else ();
              if j < w-1 then set_bin return.(i).(j+1) 1 true else ();
              if i > 0 then set_bin return.(i-1).(j) 2 true else ();
              if i < h-1 then set_bin return.(i+1).(j) 0 true else ())
           else ();
         done
       done;
       {width=w; height=h; content=fill_map return}*)
  
let isValid_map m = (*checks walls consistency*)
  let rec tmp m x y =
    if x >= m.width then tmp m 0 (y+1) else
    if y >= m.height then true else 
      let c = m.content.(y).(x) in
      let b1 =
        match (x = m.width-1, y = m.height-1) with
        | (true, true) -> true
        | (true, false) -> get_bin c 2 = get_bin m.content.(y+1).(x) 0
        | (false, true) -> get_bin c 3 = get_bin m.content.(y).(x+1) 1
        | (false, false) -> get_bin c 2 = get_bin m.content.(y+1).(x) 0 && get_bin c 3 = get_bin m.content.(y).(x+1) 1
      in b1 && tmp m (x+1) y
  in tmp m 0 0
           
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
         | 0 -> (Buffer.add_string b1 "      ";
                 Buffer.add_string b2 "      ")
         | -1 -> (Buffer.add_string b1 "&&&&&&";
                  Buffer.add_string b2 "&&&&&&")
         | i -> (Buffer.add_string b1 ("!!P" ^ (string_of_int i) ^ "!!");
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
    if m.content.(y).(x).content = 0 then false else tmp (x+1) y
  in tmp 0 0

(*place a 'side' wall in the '(col, row)' cell and if it's full set content to 'content'
 * raise Invalid_argument exception if the cell is not empty or if side is not between 0 and 3
*)
let place_wall m row col (side : int) (content : int) = 
  let c = m.content.(row).(col) in
  if not (c.content = 0) then raise (Invalid_argument "cell is not empty")
  else
    (match side with
     | 0 -> if row = 0 then () else set_bin m.content.(row-1).(col) 2 true
     | 1 -> if col = 0 then () else set_bin m.content.(row).(col-1) 3 true
     | 2 -> if row = m.height-1 then () else set_bin m.content.(row+1).(col) 0 true
     | 3 -> if col = m.width-1 then () else set_bin m.content.(row).(col+1) 1 true
     | _ -> raise (Invalid_argument "side must be between 0 and 3"));
  set_bin c side true;
  if c.bin = "1111" then c.content <- content else ()