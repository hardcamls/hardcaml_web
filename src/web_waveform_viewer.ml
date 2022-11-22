open! Base
open Brr
module Bits = Hardcaml.Bits

let render_clock = Binary_signal_renderer.render_clock
let render_bit = Binary_signal_renderer.render_bit
let render_non_binary = Non_binary_signal_renderer.render
let sprintf = Printf.sprintf

let rec render_wave (env : Env.t) (wave : Hardcaml_waveterm.Expert.Wave.t) =
  match wave with
  | Clock name -> Some (render_clock ~name)
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
       Some (render_non_binary ~name ~data ~wave_format))
  | Empty _ -> None
;;

let render (waveform : Hardcaml_waveterm.Waveform.t) =
  let open El in
  let env = { Env.current_cycle = 0 } in
  let waves = Hardcaml_waveterm.Waveform.waves waveform in
  let rows = Array.to_list waves |> List.filter_map ~f:(render_wave env) in
  table
    [ p [ txt' (sprintf "Current cycle = %d" 0) ]
    ; thead [ th [ txt' "Signals" ]; th [ txt' "Waves" ] ]
    ; tbody rows
    ]
;;
