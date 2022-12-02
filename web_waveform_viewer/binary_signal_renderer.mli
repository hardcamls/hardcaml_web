module Clock : sig
  type t

  val create : Env.t -> update_view:(unit -> unit) -> name:string -> t
  val wave_row : t -> Brr.El.t Wave_row.t
  val resize : t -> unit
  val redraw : t -> unit
end

module Bit : sig
  type t

  val create
    :  Env.t
    -> update_view:(unit -> unit)
    -> name:string
    -> data:Hardcaml_waveterm.Expert.Data.t
    -> t

  val wave_row : t -> Brr.El.t Wave_row.t
  val resize : t -> unit
  val redraw : t -> unit
end
