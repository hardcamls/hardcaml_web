open Brr
open Brr_canvas
open Hardcaml

val draw_selected_cycle : Env.t -> Canvas.t -> unit

val update_current_cycle_on_click
  :  canvas_el:El.t
  -> update_view:(unit -> unit)
  -> env:Env.t
  -> unit

val wave_data_get_opt : Hardcaml_waveterm.Expert.Data.t -> int -> Bits.t option
val clear_canvas : Env.t -> C2d.t -> unit
val create_wave_canvas : Env.t -> Canvas.t
val value_column : El.t list -> El.t
val signal_column : El.t list -> El.t
val wave_column : El.t list -> El.t
