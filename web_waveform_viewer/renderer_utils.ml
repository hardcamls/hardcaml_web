open Brr
open Brr_canvas

let draw_selected_cycle (env : Env.t) (canvas : Canvas.t) =
  let ctx = C2d.get_context canvas in
  let path =
    let path = C2d.Path.create () in
    let x =
      Constants.x_offset_to_start_of_signal
      +. Float.of_int
           ((env.selected_cycle - env.starting_cycle) * 2 * env.half_cycle_width)
    in
    C2d.Path.move_to path ~x ~y:0.0;
    C2d.Path.line_to path ~x ~y:(Float.of_int env.canvas_height);
    path
  in
  C2d.set_line_width ctx 10.0;
  C2d.set_stroke_style ctx (C2d.color (Jstr.v "blue"));
  C2d.stroke ctx path
;;

let update_current_cycle_on_click ~canvas_el ~update_view ~(env : Env.t) =
  ignore
    (Brr.Ev.listen
       Ev.click
       (fun (ev : Ev.Mouse.t Ev.t) ->
         let cycle_offset =
           let ev = Ev.as_type ev in
           let mouse_x =
             Ev.Mouse.offset_x ev *. Float.of_int Constants.canvas_scaling_factor
           in
           Float.to_int (mouse_x -. Constants.x_offset_to_start_of_signal)
           / (2 * env.half_cycle_width)
         in
         Env.update_selected_cycle env (env.starting_cycle + cycle_offset);
         update_view ())
       (Ev.target_of_jv (El.to_jv canvas_el))
      : Ev.listener)
;;

let wave_data_get_opt data i =
  let len = Hardcaml_waveterm.Expert.Data.length data in
  if i < 0 || i >= len then None else Some (Hardcaml_waveterm.Expert.Data.get data i)
;;

let create_wave_canvas (env : Env.t) =
  let canvas = Canvas.create ~w:env.canvas_width ~h:env.canvas_height [] in
  let canvas_el = Canvas.to_el canvas in
  (* For whatever reason, "block" gets rid of the spacing below the canvas in
     the td.
  *)
  El.set_inline_style (Jstr.v "display") (Jstr.v "block") canvas_el;
  canvas
;;

let clear_canvas (env : Env.t) ctx =
  C2d.clear_rect
    ctx
    ~x:0.0
    ~y:0.0
    ~w:(Float.of_int env.canvas_width)
    ~h:(Float.of_int env.canvas_height)
;;

let td xs =
  (* XXX fyquah: We should really be using CSS for this. *)
  let e = El.td xs in
  El.set_inline_style (Jstr.v "padding-top") (Jstr.v "0px") e;
  El.set_inline_style (Jstr.v "padding-bottom") (Jstr.v "0px") e;
  El.set_inline_style (Jstr.v "padding-left") (Jstr.v "10px") e;
  El.set_inline_style (Jstr.v "padding-right") (Jstr.v "10px") e;
  e
;;

let value_column xs =
  let e = td xs in
  El.set_inline_style (Jstr.v "text-align") (Jstr.v "right") e;
  e
;;

let signal_column xs =
  let e = td xs in
  e
;;

let wave_column xs = td xs
