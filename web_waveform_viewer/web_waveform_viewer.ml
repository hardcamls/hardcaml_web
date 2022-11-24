open! Base
open Brr
module Bits = Hardcaml.Bits

let sprintf = Printf.sprintf

let rec create_view_for_wave
  ~update_view
  (env : Env.t)
  (wave : Hardcaml_waveterm.Expert.Wave.t)
  =
  let create_bit ~name ~data =
    Some
      (View_element.T
         { impl = (module Binary_signal_renderer.Bit)
         ; handle = Binary_signal_renderer.Bit.create env ~update_view ~name ~data
         })
  in
  match wave with
  | Clock name ->
    Some
      (View_element.T
         { impl = (module Binary_signal_renderer.Clock)
         ; handle = Binary_signal_renderer.Clock.create env ~update_view ~name
         })
  | Binary (name, data) -> create_bit ~name ~data
  | Data (name, data, wave_format, _alignment) ->
    (match wave_format with
     | Bit -> create_bit ~name ~data
     | Bit_or wave_format ->
       if Hardcaml_waveterm.Expert.Data.length data > 0
          && Bits.width (Hardcaml_waveterm.Expert.Data.get data 0) = 1
       then create_bit ~name ~data
       else
         create_view_for_wave
           ~update_view
           env
           (Data (name, data, wave_format, _alignment))
     | Binary | Hex | Unsigned_int | Int | Index _ | Custom _ ->
       Some
         (View_element.T
            { impl = (module Non_binary_signal_renderer)
            ; handle =
                Non_binary_signal_renderer.create
                  ~update_view
                  ~name
                  ~data
                  ~wave_format
                  env
            }))
  | Empty _ -> None
;;

let create_update_starting_cycle_button ~update_view (env : Env.t) incr_or_decr =
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
       | `Incr -> env.starting_cycle <- env.starting_cycle + 1
       | `Decr -> env.starting_cycle <- Int.max 0 (env.starting_cycle - 1));
      update_view ())
    (Ev.target_of_jv (El.to_jv btn));
  btn
;;

let create_zoom_button ~update_view (env : Env.t) in_or_out =
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
      update_view ())
    (Ev.target_of_jv (El.to_jv btn));
  btn
;;

let render (waveform : Hardcaml_waveterm.Waveform.t) =
  let open El in
  let env = Env.create () in
  let waves = Hardcaml_waveterm.Waveform.waves waveform in
  let counters_div = div [] in
  let rec update_view () =
    El.set_children
      counters_div
      [ p [ txt' (sprintf "Current cycle = %d" env.starting_cycle) ]
      ; p [ txt' (sprintf "Selected cycle = %d" env.selected_cycle) ]
      ];
    List.iter (Lazy.force views_for_waves) ~f:View_element.redraw
  and views_for_waves =
    lazy
      (Array.to_list waves |> List.filter_map ~f:(create_view_for_wave ~update_view env))
  in
  let views_for_waves = Lazy.force views_for_waves in
  update_view ();
  div
    [ p
        [ create_update_starting_cycle_button env `Decr ~update_view
        ; create_update_starting_cycle_button env `Incr ~update_view
        ; create_zoom_button ~update_view env `In
        ; create_zoom_button ~update_view env `Out
        ]
    ; div
        [ counters_div
        ; table
            [ thead [ th [ txt' "Signals" ]; th [ txt' "Values" ]; th [ txt' "Waves" ] ]
            ; tbody (List.map ~f:View_element.el views_for_waves)
            ]
        ]
    ]
;;
