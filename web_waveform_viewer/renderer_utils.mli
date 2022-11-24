open Brr
open Brr_canvas

val draw_selected_cycle : Env.t -> Canvas.t -> unit

val update_current_cycle_on_click
  :  canvas_el:El.t
  -> update_view:(unit -> unit)
  -> env:Env.t
  -> unit
