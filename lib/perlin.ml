open Stdlib

(* Ce module permet de générer des grilles de booléens aléatoires de manière 
 * procédurale en utilisant du bruit de Perlin.
 * cf : https://en.wikipedia.org/wiki/Perlin_noise *)

let fade (t : float) = t *. t *. t *. (t *. (t *. 6. -. 15.) +. 10.)

let lin_interp t a b = a +. t *. (b -. a)

type vector2 = {x:float; y:float}

let generate_gradients w h = 
	Array.init h (fun _ -> Array.init w (fun _ -> let t = Random.float (2. *. Float.pi) in {x=cos(t); y=sin(t)}))

let dot_product_grad ix iy x y gradients =
	let dx = x -. float_of_int ix in
	let dy = y -. float_of_int iy in
	let grad = gradients.(ix mod (Array.length gradients)).(iy mod (Array.length gradients.(0))) in
	dx *. grad.x +. dy *. grad.y

let perlin_noise x y gradients =
	let fx = floor x in
	let fy = floor y in
	let sx = fade (x -. fx) in
	let sy = fade (y -. fy) in
	let dp1 = dot_product_grad (int_of_float fx) (int_of_float fy) x y gradients in
	let dp2 = dot_product_grad (int_of_float (fx+.1.)) (int_of_float fy) x y gradients in
	let rx1 = lin_interp sx dp1 dp2 in
	let dp3 = dot_product_grad (int_of_float fx) (int_of_float (fy+.1.)) x y gradients in
	let dp4 = dot_product_grad (int_of_float (fx+.1.)) (int_of_float (fy+.1.)) x y gradients in
	let rx2 = lin_interp sx dp3 dp4 in
	lin_interp sy rx1 rx2

let perlin_noise_grid w h scale =
	let gradients = generate_gradients w h in 
	Array.init h (fun i -> Array.init w (fun j -> perlin_noise ((float_of_int i)/.scale) ((float_of_int j)/.scale) gradients))

let round (t : float) = 
	if t -. floor t < 0.5 then floor t else ceil t

let bool_of_float t = 
	if t > 0. then true else false

let perlin_noise_grid_bool w h s = 
	Array.map (fun arr -> Array.map (fun v -> v > 0.) arr) (perlin_noise_grid w h s) 