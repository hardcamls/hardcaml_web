type t =
  { mutable starting_cycle : int
  ; mutable selected_cycle : int
  ; mutable half_cycle_width : int
  ; mutable signal_height : int
  ; mutable canvas_height : int
  ; mutable canvas_width : int
  }
[@@deriving fields]

let create () =
  { starting_cycle = 0
  ; selected_cycle = 0
  ; half_cycle_width = 30 * Constants.canvas_scaling_factor
  ; signal_height = 30 * Constants.canvas_scaling_factor
  ; canvas_width = 1000 * Constants.canvas_scaling_factor
  ; canvas_height = 50 * Constants.canvas_scaling_factor
  }
;;

let num_cycles_to_render (t : t) =
  let divisor = t.half_cycle_width * 2 in
  (t.canvas_width + divisor - 1) / divisor
;;

let update_zoom t in_or_out =
  let next_half_cycle_width =
    match in_or_out with
    | `In -> t.half_cycle_width * 2
    | `Out -> t.half_cycle_width / 2
  in
  t.half_cycle_width <- Int.max 1 (Int.min next_half_cycle_width 1000)
;;

let canvas_height_in_pixels env =
  Float.of_int env.canvas_height /. Float.of_int Constants.canvas_scaling_factor
;;

let canvas_width_in_pixels env =
  Float.of_int env.canvas_width /. Float.of_int Constants.canvas_scaling_factor
;;
