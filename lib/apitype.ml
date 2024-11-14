
(* Implémentation de tout les types du projet *)
type side = N | O | S | E
type celltype = Void | Block | CompletedBy of int
type 'a grid = 'a array array
type cell = {walls: bool ref array; mutable ctype:celltype} (*N O S E*)
type map = {width:int; height:int; content: cell grid}
type play = int * int * side


type game_view = map
type bot = game_view -> play
type player = 
| Player of int 
| Bot of int * bot
type game_state = {
	score : int array; 
	player_list : player list; 
	cur_player : player; 
	map : map }
type outcome = 
| Next of game_state
| Error of (player * string)
| Endgame of player option