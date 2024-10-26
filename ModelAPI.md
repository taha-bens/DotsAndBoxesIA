```ocaml
module type api : sig
    type game_state ok
    type game_view ~ 
    type player ok 
    type play   ok 
    type error 
    type outcome = 
        | Next of game_state
        | Error of error
        | Endgame of player option
    type bot : game_view -> play

    val view : game_state -> player -> game_view
    val display : game_view -> unit
    val act : player -> play -> game_state -> outcome
end
```


