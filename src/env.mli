type t =
  { mutable current_cycle : int
  ; mutable half_cycle_width : int
  ; mutable signal_height : int
  ; mutable canvas_height : int
  ; mutable canvas_width : int
  }
[@@deriving fields]

val create : unit -> t
val num_cycles_to_render : t -> int
val update_half_cycle_width : t -> delta:int -> unit
