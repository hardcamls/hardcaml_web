type t

val create : x:float -> y:float -> t
val rise : t -> unit
val right : t -> unit
val fall : t -> unit
val stroke : Brr_canvas.C2d.t -> t -> unit
val step : t -> bool -> unit
