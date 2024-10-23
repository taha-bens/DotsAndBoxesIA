open QCheck
open Dnb.Player

  
(* Générateur de move *)
let move_gen alpha n =
  Gen.map3 (fun x y z -> Move (x,y,z))
    (Gen.char_range 'A' alpha)
    (Gen.int_range 1 n)
    (Gen.oneof [Gen.return 'N'; Gen.return 'O'; Gen.return 'S'; Gen.return 'E'])


