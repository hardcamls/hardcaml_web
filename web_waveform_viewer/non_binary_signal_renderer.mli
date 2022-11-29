type t

val wave_row : t -> Wave_row.t
val redraw : t -> unit

val create
  :  name:string
  -> data:Hardcaml_waveterm.Expert.Data.t
  -> wave_format:Hardcaml_waveterm.Wave_format.t
  -> alignment:Hardcaml_waveterm.Text_alignment.t
  -> update_view:(unit -> unit)
  -> Env.t
  -> t
