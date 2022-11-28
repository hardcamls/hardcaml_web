type t =
  { mutable starting_cycle : int
  ; mutable selected_cycle : int
  ; mutable half_cycle_width : int
  ; mutable value_box_height : int
  ; mutable binary_signal_height : int
  ; mutable canvas_height : int
  ; mutable canvas_width : int
  ; num_cycles_in_waveform : int
  ; waveform : Hardcaml_waveterm.Waveform.t
  }
[@@deriving fields]

val create : Hardcaml_waveterm.Waveform.t -> t
val num_cycles_to_render : t -> int
val update_zoom : t -> [ `In | `Out ] -> unit
val canvas_height_in_pixels : t -> float
val canvas_width_in_pixels : t -> float
val update_selected_cycle : t -> int -> unit
val update_starting_cycle_with_delta : t -> delta:int -> unit
val update_starting_cycle_to_begin : t -> unit
val update_starting_cycle_to_end : t -> unit
