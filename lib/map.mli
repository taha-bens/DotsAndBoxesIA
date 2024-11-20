val grids_equal :
  'a Apitype.grid -> 'a Apitype.grid -> ('a -> 'a -> bool) -> bool
val cells_equal : Apitype.cell -> Apitype.cell -> bool
val maps_equal :
  Apitype.map ->
  Apitype.map -> (Apitype.cell -> Apitype.cell -> bool) -> bool
val get_cell : int -> int -> Apitype.map -> Apitype.cell
val get_walls : Apitype.cell -> bool ref array
val get_wall : Apitype.cell -> Apitype.side -> bool ref
val get_wall_val : Apitype.cell -> Apitype.side -> bool
val set_wall_val : Apitype.cell -> Apitype.side -> bool -> unit
val set_wall_ref : Apitype.cell -> Apitype.side -> bool ref -> unit
val get_ctype : Apitype.cell -> Apitype.celltype
val set_ctype : Apitype.cell -> Apitype.celltype -> unit
val is_full_cell : Apitype.cell -> bool
val is_full : Apitype.map -> bool
val convert_to_block : Apitype.cell -> unit
val empty_cell : unit -> Apitype.cell
val get_NO_neighbors : int -> int -> Apitype.map -> Apitype.cell array
val empty_map : int -> int -> Apitype.map
val fill_map : Apitype.map -> Apitype.map
val copy_map : Apitype.map -> Apitype.map
val get_unwalled_side : Apitype.cell -> Apitype.side option
val random_map : int -> int -> Apitype.map
val perlin_map : int -> int -> Apitype.map
val buf_of_line : int -> Apitype.map -> Buffer.t
val buf_of_map : Apitype.map -> Buffer.t
val is_legal : Apitype.map -> Apitype.play -> bool
val apply_play : Apitype.map -> Apitype.play -> int -> int
