


let clear_screen () =
  print_string "\027[2J";
  print_string "\027[H";
  flush stdout


let string_box (s : string) = 
  clear_screen ();
  print_string ((String.make ((String.length s) + 5) '#') ^ "\n");
  print_string s



let print_message (s : string) = string_box s
