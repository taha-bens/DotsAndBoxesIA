(* open QCheck
open Dnb.Player

  
(* Générateur de move *)
let move_gen alpha n =
  Gen.map3 (fun x y z -> Move (x,y,z))
    (Gen.char_range 'A' alpha)
    (Gen.int_range 1 n)
    (Gen.oneof [Gen.return 'N'; Gen.return 'O'; Gen.return 'S'; Gen.return 'E'])


(* générateur de players pas du tout intéligent est qui joue toujour le même coup *)
let player_gen (map : Dnb.Map.map) : Dnb.Player.player Gen.t = 

  let c = (match Dnb.Game.nth_letter map.width with None -> ' ' | Some c -> c) in 
  let h = map.height in 

  Gen.map2 (fun id move -> (id,fun _ -> move)) (Gen.int_range 1 10) (move_gen c h)


 *)