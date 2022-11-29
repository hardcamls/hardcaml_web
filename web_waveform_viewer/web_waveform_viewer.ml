open! Base
open Brr
module Bits = Hardcaml.Bits
module Display_rule = Hardcaml_waveterm.Display_rule
module Display_rules = Hardcaml_waveterm.Display_rules
module Waveform = Hardcaml_waveterm.Waveform

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
  | Data (name, data, wave_format, alignment) ->
    (match wave_format with
     | Bit -> create_bit ~name ~data
     | Bit_or wave_format ->
       if Hardcaml_waveterm.Expert.Data.length data > 0
          && Bits.width (Hardcaml_waveterm.Expert.Data.get data 0) = 1
       then create_bit ~name ~data
       else
         create_view_for_wave ~update_view env (Data (name, data, wave_format, alignment))
     | Binary | Hex | Unsigned_int | Int | Index _ | Custom _ ->
       Some
         (View_element.T
            { impl = (module Non_binary_signal_renderer)
            ; handle =
                Non_binary_signal_renderer.create
                  ~alignment
                  ~update_view
                  ~name
                  ~data
                  ~wave_format
                  env
            }))
  | Empty _ -> None
;;

module Update_starting_cycle_action = struct
  type t =
    | Delta of
        { icon : string
        ; delta : int
        }
    | Fast_forward of string
    | Fast_backward of string
end

let create_update_starting_cycle_button ~update_view (env : Env.t) ~action =
  let btn =
    let open El in
    button
      [ txt'
          (match action with
           | Update_starting_cycle_action.Delta { icon; delta = _ } -> icon
           | Fast_forward icon | Fast_backward icon -> icon)
      ]
  in
  Ev.listen
    Ev.click
    (fun (_ : Ev.Mouse.t Ev.t) ->
      (match action with
       | Delta { icon = _; delta } -> Env.update_starting_cycle_with_delta env ~delta
       | Fast_forward _ -> Env.update_starting_cycle_to_end env
       | Fast_backward _ -> Env.update_starting_cycle_to_begin env);
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
  Ev.listen
    Ev.click
    (fun (_ : Ev.Mouse.t Ev.t) ->
      Env.update_zoom env in_or_out;
      update_view ())
    (Ev.target_of_jv (El.to_jv btn));
  btn
;;

let render
  ~(display_rules : Display_rules.t option)
  (waveform : Hardcaml_waveterm.Waveform.t)
  =
  let open El in
  let env = Env.create waveform in
  let waves = Waveform.sort_ports_and_formats waveform display_rules in
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
  let waves_table =
    table
      [ thead [ th [ txt' "Signals" ]; th [ txt' "Values" ]; th [ txt' "Waves" ] ]
      ; tbody
          (List.map views_for_waves ~f:(fun view ->
             let { Wave_row.signal_column; value_column; wave_column } =
               View_element.wave_row view
             in
             El.tr [ signal_column; value_column; wave_column ]))
      ]
  in
  El.set_at (Jstr.v "cellspacing") (Some (Jstr.v "0")) waves_table;
  El.set_inline_style (Jstr.v "border-collapse") (Jstr.v "collapse") waves_table;
  El.set_inline_style (Jstr.v "border-spacing") (Jstr.v "0") waves_table;
  div
    [ p
        [ create_update_starting_cycle_button
            env
            ~action:(Fast_backward "|<<")
            ~update_view
        ; create_update_starting_cycle_button
            env
            ~action:(Delta { icon = "<<"; delta = -10 })
            ~update_view
        ; create_update_starting_cycle_button
            env
            ~action:(Delta { icon = "<"; delta = -1 })
            ~update_view
        ; create_update_starting_cycle_button
            env
            ~action:(Delta { icon = ">"; delta = 1 })
            ~update_view
        ; create_update_starting_cycle_button
            env
            ~action:(Delta { icon = ">>"; delta = 10 })
            ~update_view
        ; create_update_starting_cycle_button
            env
            ~action:(Fast_forward ">>|")
            ~update_view
        ; create_zoom_button ~update_view env `In
        ; create_zoom_button ~update_view env `Out
        ]
    ; div [ counters_div; waves_table ]
    ]
;;
