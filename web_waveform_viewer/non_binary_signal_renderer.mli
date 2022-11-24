val render
  :  name:string
  -> data:Hardcaml_waveterm.Expert.Data.t
  -> wave_format:Hardcaml_waveterm.Wave_format.t
  -> update_view:(unit -> unit)
  -> Env.t
  -> Brr.El.t
