open Brr
open Brr_canvas

let draw_selected_cycle (env : Env.t) (canvas : Canvas.t) =
  let ctx = C2d.get_context canvas in
  let path =
    let path = C2d.Path.create () in
    let x =
      Constants.x_offset_to_start_of_signal
      +. Float.of_int (env.selected_cycle * 4 * env.half_cycle_width)
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
  Brr.Ev.listen
    Ev.click
    (fun (ev : Ev.Mouse.t Ev.t) ->
      let cycle_offset =
        let ev = Ev.as_type ev in
        let mouse_x = Ev.Mouse.offset_x ev in
        (* fyquah: Idk why multiply by 4, but that seems to work *)
        Float.to_int (mouse_x -. Constants.x_offset_to_start_of_signal)
        * Constants.canvas_scaling_factor
        / (4 * env.half_cycle_width)
      in
      env.selected_cycle <- env.starting_cycle + cycle_offset;
      update_view ())
    (Ev.target_of_jv (El.to_jv canvas_el))
;;
