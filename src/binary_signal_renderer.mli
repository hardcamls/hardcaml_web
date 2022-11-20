type t

val create : x:float -> y:float -> t

val rise_and_stroke_right
  :  half_cycle_width:float
  -> binary_signal_height:float
  -> t
  -> unit

val fall_and_stroke_right
  :  half_cycle_width:float
  -> binary_signal_height:float
  -> t
  -> unit

val stroke : Brr_canvas.C2d.t -> t -> unit
