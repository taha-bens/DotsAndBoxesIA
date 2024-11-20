open OUnit2
open Stdlib
open Dnb.Perlin

(* Tests pour la fonction fade *)
let test_fade _ =
  assert_equal (fade 0.0) 0.0 ~msg:"fade(0.0) should be 0.0";
  assert_equal (fade 1.0) 1.0 ~msg:"fade(1.0) should be 1.0";
  assert_bool "fade(0.5) should be positive" (fade 0.5 > 0.0)

(* Tests pour la fonction lin_interp *)
let test_lin_interp _ =
  assert_equal (lin_interp 0.0 0.0 1.0) 0.0 ~msg:"lin_interp(0.0, 0.0, 1.0) should be 0.0";
  assert_equal (lin_interp 1.0 0.0 1.0) 1.0 ~msg:"lin_interp(1.0, 0.0, 1.0) should be 1.0";
  assert_equal (lin_interp 0.5 0.0 1.0) 0.5 ~msg:"lin_interp(0.5, 0.0, 1.0) should be 0.5"

(* Tests pour la génération des gradients *)
let test_generate_gradients _ =
  let gradients = generate_gradients 4 4 in
  assert_equal (Array.length gradients) 4 ~msg:"Grid height should be 4";
  assert_equal (Array.length gradients.(0)) 4 ~msg:"Grid width should be 4";
  Array.iter (fun row -> Array.iter (fun g -> 
    assert_bool "Gradient vectors should have valid components" 
      ((g.x *. g.x +. g.y *. g.y) > 0.9 && (g.x *. g.x +. g.y *. g.y) < 1.1)
  ) row) gradients

(* Tests pour le produit scalaire avec gradients *)
let test_dot_product_grad _ =
  let gradients = generate_gradients 2 2 in
  let dp = dot_product_grad 0 0 0.5 0.5 gradients in
  assert_bool "Dot product should be a finite number" (Float.is_finite dp)

(* Tests pour le bruit de Perlin *)
let test_perlin_noise _ =
  let gradients = generate_gradients 2 2 in
  let noise = perlin_noise 0.5 0.5 gradients in
  assert_bool "Perlin noise value should be finite" (Float.is_finite noise)

(* Tests pour la génération de la grille de bruit de Perlin *)
let test_perlin_noise_grid _ =
  let grid = perlin_noise_grid 4 4 1.0 in
  assert_equal (Array.length grid) 4 ~msg:"Grid height should be 4";
  assert_equal (Array.length grid.(0)) 4 ~msg:"Grid width should be 4";
  Array.iter (fun row -> Array.iter (fun v -> 
    assert_bool "Perlin noise values should be finite" (Float.is_finite v)
  ) row) grid

(* Tests pour la génération de la grille de booléens *)
let test_perlin_noise_grid_bool _ =
  let grid = perlin_noise_grid_bool 4 4 1.0 in
  assert_equal (Array.length grid) 4 ~msg:"Grid height should be 4";
  assert_equal (Array.length grid.(0)) 4 ~msg:"Grid width should be 4";
  Array.iter (fun row -> Array.iter (fun v -> 
    assert_bool "Grid values should be boolean" (v = true || v = false)
  ) row) grid