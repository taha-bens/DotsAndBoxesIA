open OUnit2

let assert_custom_equal (cmp : 'a -> 'a -> bool) (exp : 'a) (act : 'a) =
  if not (cmp exp act) then
    failwith (Printf.sprintf "Assertion failed: \nExpected: %s\nActual: %s")
              (*(Printexc.to_string (Failure expected))
              (Printexc.to_string (Failure actual)))*)
  else
    print_endline "Test passed."