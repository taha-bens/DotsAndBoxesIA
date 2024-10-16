
(* Module des futures IA, C'est ici que le programme viens chercher la liste de joueurs *)
let (players : Game.player list) = [
  (1,fun _ -> ("F1",Game.Block));
  (2,fun _ -> ("C4", Game.Block))
  ]