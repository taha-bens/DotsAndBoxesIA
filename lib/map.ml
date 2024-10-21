Random.init;;

type cell = {mutable bin:string; mutable content:int} (*Norme binaire : NOSE*)
            
type map = {width:int; height:int; content: cell array array} (* width and height need to be >= 2 *)
           
let rfb (s : string) (i : int) = (*'read formatted binary' - format : "0100", 0 <= i <= 3*)
  let c = String.get s i in 
  match c with
  | '0' -> false
  | '1' -> true
  | _ -> raise (Invalid_argument "wrong string format")
           
let modify_string s i c = (* strings are not mutable in OCaml... :-(  *)
  let arr = Array.of_list (List.init (String.length s) (String.get s)) in
  arr.(i) <- c;
  String.init (Array.length arr) (Array.get arr)
           
let wfb (s : string) (i : int) (v : bool) = 
  let c = if v then '1' else '0' in
  if i >= 0 && i < 4 then modify_string s i c else 
    raise (Invalid_argument "index out of bounds : required 0 <= i < 4")
      
let set_bin (c : cell) (i : int) (v : bool) =
  c.bin <- wfb c.bin i v
      
let get_bin (c : cell) (i : int) = 
  rfb c.bin i

let fill_map m =
  for i = 0 to m.height -1 do
    for j = 0 to m.width -1 do
      if m.content.(i).(j).bin = "1111" then m.content.(i).(j).content <- -1
    done
  done;
  m
  
let random_map w h =
  let return : cell array array = Array.init h (fun _ -> Array.init w (fun _ -> {bin="0000"; content= 0})) in
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      if Random.bool () && Random.bool () then 
        (return.(i).(j) <- {bin="1111"; content= -1};
         if j > 0 then set_bin return.(i).(j-1) 3 true else ();
         if j < w-1 then set_bin return.(i).(j+1) 1 true else ();
         if i > 0 then set_bin return.(i-1).(j) 2 true else ();
         if i < h-1 then set_bin return.(i+1).(j) 0 true else ())
      else ();
      if i = 0 then set_bin return.(i).(j) 0 true else ();
      if i = h-1 then set_bin return.(i).(j) 2 true else ();
      if j = 0 then set_bin return.(i).(j) 1 true else ();
      if j = w-1 then set_bin return.(i).(j) 3 true else ();
    done
  done;
  fill_map {width=w; height=h; content=return}

let perlin_map w h =
  let return = Array.map (fun arr -> Array.map (fun b -> if b then {bin="1111"; content= -1} else {bin="0000"; content=0}) arr) (Perlin.perlin_noise_grid_bool w h 2.) in
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      if return.(i).(j).content = -1 then 
        (if j > 0 then set_bin return.(i).(j-1) 3 true else ();
         if j < w-1 then set_bin return.(i).(j+1) 1 true else ();
         if i > 0 then set_bin return.(i-1).(j) 2 true else ();
         if i < h-1 then set_bin return.(i+1).(j) 0 true else ())
      else ();
      if i = 0 then set_bin return.(i).(j) 0 true else ();
      if i = h-1 then set_bin return.(i).(j) 2 true else ();
      if j = 0 then set_bin return.(i).(j) 1 true else ();
      if j = w-1 then set_bin return.(i).(j) 3 true else ();
    done
  done;
  {width=w; height=h; content=fill_map return}
  
let isValid_map m =
  let rec tmp m x y =
    if x >= m.width then tmp m 0 (y+1) else
    if y >= m.height then true else 
      let c = m.content.(y).(x) in
      let b1 = 
        if x = 0 then (get_bin c 1 && (if y = 0 then get_bin c 0 else true))
        else if y = 0 then get_bin c 0 else true
      in let b2 =
           if x = m.width-1 then 
             (get_bin c 3 && 
              (if y = m.height-1 then get_bin c 2 
               else (get_bin c 2 = get_bin m.content.(y+1).(x) 0)))
           else if y = m.height-1 then 
             ((get_bin c 2) && 
              (get_bin c 3 = get_bin m.content.(y).(x+1) 1)) 
           else ((get_bin c 2 = get_bin m.content.(y+1).(x) 0) && 
                 (get_bin c 3 = get_bin m.content.(y).(x+1) 1))
      in b1 && b2 && tmp m (x+1) y
  in tmp m 0 0
           
let print_line_map m l =
  if l >= m.height then () else
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
    
let print_map m =
  let rec tmp i = 
    if i >= m.height then () else
      (print_line_map m i; tmp (i+1))
  in tmp 0