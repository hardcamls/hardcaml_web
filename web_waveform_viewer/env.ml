open Core
module Wave = Hardcaml_waveterm.Expert.Wave
module Data = Hardcaml_waveterm.Expert.Data

type t =
  { mutable starting_cycle : int
  ; mutable selected_cycle : int
  ; mutable half_cycle_width : int
  ; mutable signal_height : int
  ; mutable canvas_height : int
  ; mutable canvas_width : int
  ; num_cycles_in_waveform : int
  ; waveform : Hardcaml_waveterm.Waveform.t
  }
[@@deriving fields]

let create waveform =
  let num_cycles_in_waveform =
    waveform
    |> Hardcaml_waveterm.Waveform.waves
    |> Array.fold ~init:0 ~f:(fun acc wave ->
         match wave with
         | Clock _ | Empty _ -> acc
         | Binary (_, data) | Data (_, data, _, _) -> Int.max acc (Data.length data))
  in
  { starting_cycle = 0
  ; selected_cycle = 0
  ; half_cycle_width = 30 * Constants.canvas_scaling_factor
  ; signal_height = 30 * Constants.canvas_scaling_factor
  ; canvas_width = 1000 * Constants.canvas_scaling_factor
  ; canvas_height = 50 * Constants.canvas_scaling_factor
  ; num_cycles_in_waveform
  ; waveform
  }
;;

let num_cycles_that_can_fit_in_canvas (t : t) =
  let divisor = t.half_cycle_width * 2 in
  (t.canvas_width + divisor - 1) / divisor
;;

let num_cycles_to_render (t : t) =
  Int.min
    (num_cycles_that_can_fit_in_canvas t)
    (t.num_cycles_in_waveform - t.starting_cycle)
;;

let update_zoom t in_or_out =
  let next_half_cycle_width =
    match in_or_out with
    | `In -> t.half_cycle_width * 2
    | `Out -> t.half_cycle_width / 2
  in
  t.half_cycle_width <- Int.max 1 (Int.min next_half_cycle_width 1000)
;;

let canvas_height_in_pixels env =
  Float.of_int env.canvas_height /. Float.of_int Constants.canvas_scaling_factor
;;

let canvas_width_in_pixels env =
  Float.of_int env.canvas_width /. Float.of_int Constants.canvas_scaling_factor
;;

let update_selected_cycle (t : t) x =
  t.selected_cycle <- Int.max 0 (Int.min x (t.num_cycles_in_waveform - 1))
;;

let clip_cycle t x = Int.max 0 (Int.min x (t.num_cycles_in_waveform - 1))

let update_starting_cycle_with_delta (t : t) ~delta =
  t.starting_cycle <- clip_cycle t (t.starting_cycle + delta)
;;

let update_starting_cycle_to_begin (t : t) = t.starting_cycle <- 0

let update_starting_cycle_to_end (t : t) =
  t.starting_cycle
    <- clip_cycle t (t.num_cycles_in_waveform - num_cycles_that_can_fit_in_canvas t)
;;
