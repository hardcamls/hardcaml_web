type t =
  { mutable starting_cycle : int
  ; mutable selected_cycle : int
  ; mutable half_cycle_width : int
  ; mutable signal_height : int
  ; mutable canvas_height : int
  ; mutable canvas_width : int
  }
[@@deriving fields]

val create : unit -> t
val num_cycles_to_render : t -> int
val update_zoom : t -> [ `In | `Out ] -> unit
val canvas_height_in_pixels : t -> float
val canvas_width_in_pixels : t -> float
