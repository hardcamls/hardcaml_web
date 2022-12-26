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
        { icon : El.t
        ; delta : Env.t -> int
        }
    | Fast_forward of El.t
    | Fast_backward of El.t
end

let create_update_starting_cycle_button ~update_view (env : Env.t) ~action =
  let btn =
    let open El in
    button
      [ (match action with
         | Update_starting_cycle_action.Delta { icon; delta = _ }
         | Fast_forward icon
         | Fast_backward icon -> icon)
      ]
  in
  ignore
    (Ev.listen
       Ev.click
       (fun (_ : Ev.Mouse.t Ev.t) ->
         (match action with
          | Delta { icon = _; delta } ->
            Env.update_starting_cycle_with_delta env ~delta:(delta env)
          | Fast_forward _ -> Env.update_starting_cycle_to_end env
          | Fast_backward _ -> Env.update_starting_cycle_to_begin env);
         update_view ())
       (Ev.target_of_jv (El.to_jv btn))
      : Ev.listener);
  btn
;;

let blur_el el = ignore (Jv.call (El.to_jv el) "blur" [||] : Jv.t)

let create_current_cycle_textfield ~update_view (env : Env.t) =
  let textfield =
    El.input ~at:[ At.class' (Jstr.v "textbox"); At.value (Jstr.v "0") ] ()
  in
  ignore
    (Ev.listen
       Ev.blur
       (fun (ev : _ Ev.t) ->
         Ev.prevent_default ev;
         let selected_cycle =
           try
             El.prop El.Prop.value textfield |> Jstr.to_string |> Int.of_string |> Some
           with
           | _ -> None
         in
         Option.iter
           selected_cycle
           ~f:(Env.update_selected_cycle_and_scroll_so_that_visible env);
         update_view ())
       (Ev.target_of_jv (El.to_jv textfield))
      : Ev.listener);
  ignore
    (Ev.listen
       Ev.keyup
       (fun (ev : _ Ev.t) ->
         (* When the user hits the enter button, blur, simulate as if the user
         cliked away
      *)
         Ev.prevent_default ev;
         if String.equal "Enter" (Jstr.to_string (Ev.Keyboard.key (Ev.as_type ev)))
         then blur_el textfield)
       (Ev.target_of_jv (El.to_jv textfield))
      : Ev.listener);
  textfield
;;

let create_zoom_button ~update_view (env : Env.t) in_or_out =
  let btn =
    let open El in
    button
      [ (match in_or_out with
         | `In -> Icons.zoom_in ()
         | `Out -> Icons.zoom_out ())
      ]
  in
  ignore
    (Ev.listen
       Ev.click
       (fun (_ : Ev.Mouse.t Ev.t) ->
         Env.update_zoom env in_or_out;
         update_view ())
       (Ev.target_of_jv (El.to_jv btn))
      : Ev.listener);
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
  let signal_width = 10 in
  let value_width = 10 in
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
  let rec update_view () =
    El.set_prop
      Prop.value
      (Jstr.v (Int.to_string env.selected_cycle))
      (Lazy.force selected_cycle_textfield);
    List.iter (Lazy.force views_for_waves) ~f:View_element.redraw
  and selected_cycle_textfield = lazy (create_current_cycle_textfield ~update_view env)
  and views_for_waves =
    lazy
      (Array.to_list waves |> List.filter_map ~f:(create_view_for_wave ~update_view env))
  in
  let views_for_waves = Lazy.force views_for_waves in
  let selected_cycle_textfield = Lazy.force selected_cycle_textfield in
  let resize_and_redraw () =
    List.iter views_for_waves ~f:View_element.resize;
    update_view ()
  in
  update_view ();
  let column_style =
    At.style (Jstr.v "padding: 5px; padding-left: 10px; padding-right: 10px;")
  in
  let row_style =
    At.style (Jstr.v "display: flex; margin-left: -5px; margin-right: -5px;")
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
                 let at = [ At.style (Jstr.v (sprintf "height: %fpx" line_height)) ] in
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
        Env.set_canvas_width_in_pixels env (w -. 50.0);
        resize_and_redraw ();
        update_view ())
  in
  set_timeout ~ms:0 ~f:update_canvas_width_on_resize;
  ignore
    (Ev.listen
       Ev.resize
       (fun (_ : _ Ev.t) -> update_canvas_width_on_resize ())
       (Ev.target_of_jv (Window.to_jv G.window))
      : Ev.listener);
  let waves_div = div [ El.div ~at:[ row_style ] columns ] in
  let delta_half_a_page env =
    Int.max 1 ((Env.num_cycles_that_can_fit_in_canvas env + 1) / 2)
  in
  div
    [ p
        [ create_update_starting_cycle_button
            env
            ~action:(Fast_backward (Icons.to_beginning_of_simulation ()))
            ~update_view
        ; create_update_starting_cycle_button
            env
            ~action:
              (Delta
                 { icon = Icons.backwards_fast ()
                 ; delta = Fn.compose Int.neg delta_half_a_page
                 })
            ~update_view
        ; create_update_starting_cycle_button
            env
            ~action:(Delta { icon = Icons.backwards_normal (); delta = Fn.const (-1) })
            ~update_view
        ; selected_cycle_textfield
        ; create_update_starting_cycle_button
            env
            ~action:(Delta { icon = Icons.forwards_normal (); delta = Fn.const 1 })
            ~update_view
        ; create_update_starting_cycle_button
            env
            ~action:(Delta { icon = Icons.forwards_fast (); delta = delta_half_a_page })
            ~update_view
        ; create_update_starting_cycle_button
            env
            ~action:(Fast_forward (Icons.to_end_of_simulation ()))
            ~update_view
        ; create_zoom_button ~update_view env `In
        ; create_zoom_button ~update_view env `Out
        ]
    ; waves_div
    ]
;;
