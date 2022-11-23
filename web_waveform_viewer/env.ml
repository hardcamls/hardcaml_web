type t =
  { mutable starting_cycle : int
  ; mutable half_cycle_width : int
  ; mutable signal_height : int
  ; mutable canvas_height : int
  ; mutable canvas_width : int
  }
[@@deriving fields]

let create () =
  { starting_cycle = 0
  ; half_cycle_width = 300
  ; signal_height = 300
  ; canvas_width = 10000
  ; canvas_height = 500
  }
;;

let num_cycles_to_render (t : t) = t.canvas_width / (t.half_cycle_width * 2)

let update_half_cycle_width t ~delta =
  t.half_cycle_width <- Int.max 100 (Int.min (t.half_cycle_width + delta) 1000)
;;
