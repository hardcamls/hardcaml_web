module Clock : sig
  type t

  val create : Env.t -> update_view:(unit -> unit) -> name:string -> t
  val el : t -> Brr.El.t
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

  val el : t -> Brr.El.t
  val redraw : t -> unit
end
