open! Base
open Brr
module Bits = Hardcaml.Bits

let render_clock = Binary_signal_renderer.render_clock
let render_bit = Binary_signal_renderer.render_bit
let render_non_binary = Non_binary_signal_renderer.render
let sprintf = Printf.sprintf

let rec render_wave (env : Env.t) (wave : Hardcaml_waveterm.Expert.Wave.t) =
  match wave with
  | Clock name -> Some (render_clock env ~name)
  | Binary (name, data) -> Some (render_bit env ~name ~data)
  | Data (name, data, wave_format, _alignment) ->
    (match wave_format with
     | Bit -> Some (render_bit env ~name ~data)
     | Bit_or wave_format ->
       if Hardcaml_waveterm.Expert.Data.length data > 0
          && Bits.width (Hardcaml_waveterm.Expert.Data.get data 0) = 1
       then Some (render_bit env ~name ~data)
       else render_wave env (Data (name, data, wave_format, _alignment))
     | Binary | Hex | Unsigned_int | Int | Index _ | Custom _ ->
       Some (render_non_binary ~name ~data ~wave_format env))
  | Empty _ -> None
;;

let create_update_current_cycle_button ~update_waves (env : Env.t) incr_or_decr =
  let btn =
    let open El in
    button
      [ txt'
          (match incr_or_decr with
           | `Incr -> ">"
           | `Decr -> "<")
      ]
  in
  Ev.listen
    Ev.click
    (fun (_ : Ev.Mouse.t Ev.t) ->
      (match incr_or_decr with
       | `Incr -> env.current_cycle <- env.current_cycle + 1
       | `Decr -> env.current_cycle <- Int.max 0 (env.current_cycle - 1));
      update_waves ())
    (Ev.target_of_jv (El.to_jv btn));
  btn
;;

let create_zoom_button ~update_waves (env : Env.t) in_or_out =
  let btn =
    let open El in
    button
      [ txt'
          (match in_or_out with
           | `In -> "Zoom In"
           | `Out -> "Zoom Out")
      ]
  in
  let delta =
    match in_or_out with
    | `In -> 50
    | `Out -> -50
  in
  Ev.listen
    Ev.click
    (fun (_ : Ev.Mouse.t Ev.t) ->
      Env.update_half_cycle_width env ~delta;
      update_waves ())
    (Ev.target_of_jv (El.to_jv btn));
  btn
;;

let render (waveform : Hardcaml_waveterm.Waveform.t) =
  let open El in
  let env = Env.create () in
  let waves = Hardcaml_waveterm.Waveform.waves waveform in
  let waves_div = div [] in
  let update_waves () =
    let rows = Array.to_list waves |> List.filter_map ~f:(render_wave env) in
    El.set_children
      waves_div
      [ p [ txt' (sprintf "Current cycle = %d" env.current_cycle) ]
      ; table [ thead [ th [ txt' "Signals" ]; th [ txt' "Waves" ] ]; tbody rows ]
      ]
  in
  update_waves ();
  div
    [ p
        [ create_update_current_cycle_button env `Decr ~update_waves
        ; create_update_current_cycle_button env `Incr ~update_waves
        ; create_zoom_button ~update_waves env `In
        ; create_zoom_button ~update_waves env `Out
        ]
    ; waves_div
    ]
;;
