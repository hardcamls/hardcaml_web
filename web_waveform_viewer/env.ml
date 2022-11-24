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
  ; half_cycle_width = 300
  ; signal_height = 300
  ; canvas_width = 10000
  ; canvas_height = 500
  }
;;

let num_cycles_to_render (t : t) =
  let divisor = t.half_cycle_width * 2 in
  (t.canvas_width + divisor - 1) / divisor
;;

let update_half_cycle_width t ~delta =
  t.half_cycle_width <- Int.max 1 (Int.min (t.half_cycle_width + delta) 1000)
;;
