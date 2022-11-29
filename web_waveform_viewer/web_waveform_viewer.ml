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

let set_scroll_left (el : El.t) (value : float) : unit =
  Jv.set (Jv.repr el) "scrollLeft" (Jv.of_float value)
;;

let set_timeout ~ms ~f =
  let (_ : unit Fut.t) =
    let open Fut.Syntax in
    let* () = Fut.tick ~ms in
    let () = f () in
    Fut.return ()
  in
  ()
;;

module Column_name = struct
  type t =
    | Signals
    | Values
    | Waves
  [@@deriving sexp_of, equal]

  let to_string x = Sexp.to_string_mach (sexp_of_t x)
end

module Column_spec = struct
  type t =
    { column_name : Column_name.t
    ; accessor : Brr.El.t Wave_row.t -> Brr.El.t
    ; flex_pc : int
    ; scroll_to_right : bool
    }
end

let column_specs =
  let open Column_spec in
  let signal_width = 5 in
  let value_width = 5 in
  [ { column_name = Signals
    ; accessor = Wave_row.signal_column
    ; flex_pc = signal_width
    ; scroll_to_right = false
    }
  ; { column_name = Values
    ; accessor = Wave_row.value_column
    ; flex_pc = value_width
    ; scroll_to_right = true
    }
  ; { column_name = Waves
    ; accessor = Wave_row.wave_column
    ; flex_pc = 100 - signal_width - value_width
    ; scroll_to_right = false
    }
  ]
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
  let column_style = At.style (Jstr.v "padding: 5px") in
  let row_style =
    At.style (Jstr.v "display: flex; margin-left: -5px; margin-right: -5px;")
  in
  let waves_header =
    List.map column_specs ~f:(fun column_spec ->
      let { Column_spec.column_name; accessor = _; flex_pc; scroll_to_right = _ } =
        column_spec
      in
      div
        ~at:[ column_style; At.style (Jstr.v (sprintf "flex: %d%%;" flex_pc)) ]
        [ b [ txt' (Column_name.to_string column_name) ] ])
    |> div ~at:[ row_style ]
  in
  let columns =
    List.map column_specs ~f:(fun column_spec ->
      let { Column_spec.column_name = _; accessor; flex_pc; scroll_to_right } =
        column_spec
      in
      let table =
        table
          [ tbody
              (List.map views_for_waves ~f:(fun view ->
                 let line_height = Env.canvas_height_in_pixels env in
                 let at =
                   [ At.style (Jstr.v (sprintf "line-height: %fpx" line_height)) ]
                 in
                 tr ~at [ accessor (View_element.wave_row view) ]))
          ]
      in
      El.set_at (Jstr.v "cellspacing") (Some (Jstr.v "0")) table;
      El.set_inline_style (Jstr.v "border-collapse") (Jstr.v "collapse") table;
      El.set_inline_style (Jstr.v "border-spacing") (Jstr.v "0") table;
      let div =
        div
          ~at:
            [ column_style
            ; At.style (Jstr.v (sprintf "flex: %d%%; overflow-x: auto;" flex_pc))
            ]
          [ table ]
      in
      if scroll_to_right
      then set_timeout ~ms:0 ~f:(fun () -> set_scroll_left div 999999.9);
      div)
  in
  let update_canvas_width_on_resize =
    let wave_column =
      let%bind.Option i, _ =
        List.findi column_specs ~f:(fun _ c -> Column_name.equal c.column_name Waves)
      in
      List.nth columns i
    in
    fun () ->
      Option.iter wave_column ~f:(fun wave_column ->
        let w = El.bound_w wave_column in
        Env.set_canvas_width_in_pixels env (w -. 30.0);
        update_view ())
  in
  set_timeout ~ms:0 ~f:update_canvas_width_on_resize;
  Ev.listen
    Ev.resize
    (fun (_ : _ Ev.t) -> update_canvas_width_on_resize ())
    (Ev.target_of_jv (Window.to_jv G.window));
  let waves_div = div [ waves_header; El.div ~at:[ row_style ] columns ] in
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
    ; div [ counters_div ]
    ; waves_div
    ]
;;
