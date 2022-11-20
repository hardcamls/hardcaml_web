open Brr_canvas
open Hardcaml

type t

val create : x:float -> y:float -> bits_to_string:(Bits.t -> string) -> C2d.t -> t
val step : t -> Bits.t -> unit
val render_last_value : t -> unit