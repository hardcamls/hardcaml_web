val render_clock : Env.t -> update_view:(unit -> unit) -> name:string -> Brr.El.t

val render_bit
  :  Env.t
  -> update_view:(unit -> unit)
  -> name:string
  -> data:Hardcaml_waveterm.Expert.Data.t
  -> Brr.El.t
