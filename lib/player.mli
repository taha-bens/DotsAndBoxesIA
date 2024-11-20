val get_player_id : Apitype.player -> int
val get_next_player : Apitype.player list -> Apitype.player -> Apitype.player
val get_player_play : unit -> Apitype.play
val string_of_player : Apitype.player -> string
val get_best_player : Apitype.game_state -> Apitype.player option
