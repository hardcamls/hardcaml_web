open Brr_canvas
open Hardcaml

type t

val create : x:float -> y:float -> C2d.t -> t
val step_with_value : t -> Bits.t -> unit
val render_last_value : t -> unit
