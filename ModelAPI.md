```ocaml
module type api : sig
    type game_state
    type game_view
    type player
    type play
    type error
    type outcome = 
        | Next of game_state
        | Error of error
        | Endgame of player option
    type bot : game_view -> play

    val view : game_state -> player -> gmae_view
    val display : game_view -> unit
    val act : player -> play -> game_state -> outcome
end
```


