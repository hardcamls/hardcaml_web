open Core
open Brr
open Brr_canvas
module Bits = Hardcaml.Bits

module Path_builder : sig
  type t

  val create : x:float -> y:float -> half_cycle_width:int -> signal_height:int -> t
  val right : t -> unit
  val step : t -> bool -> unit
  val path : t -> C2d.Path.t
end = struct
  module Last_value = struct
    type t =
      | False
      | True
      | Nothing
    [@@deriving equal]

    let of_bool = function
      | true -> True
      | false -> False
    ;;
  end

  type t =
    { path : C2d.Path.t
    ; signal_height : int
    ; half_cycle_width : int
    ; mutable x : float
    ; mutable y : float
    ; mutable last_value : Last_value.t
    }
  [@@deriving fields]

  let create ~x ~y ~half_cycle_width ~signal_height =
    let path = C2d.Path.create () in
    C2d.Path.move_to path ~x ~y;
    { path; x; y; last_value = Nothing; half_cycle_width; signal_height }
  ;;

  let line_or_move_to (t : t) ~dx ~dy ~f =
    let x = t.x +. Float.of_int dx in
    let y = t.y +. Float.of_int dy in
    f t.path ~x ~y;
    t.x <- x;
    t.y <- y
  ;;

  let line_to t ~dx ~dy = line_or_move_to t ~dx ~dy ~f:C2d.Path.line_to
  let move_to t ~dx ~dy = line_or_move_to t ~dx ~dy ~f:C2d.Path.move_to
  let rise t = line_to t ~dx:0 ~dy:(Int.neg t.signal_height)
  let right t = line_to t ~dx:t.half_cycle_width ~dy:0
  let fall t = line_to t ~dx:0 ~dy:t.signal_height

  let step (t : t) tf =
    (match t.last_value with
     | Nothing -> if not tf then move_to t ~dx:0 ~dy:t.signal_height
     | True | False ->
       if not (Last_value.equal (Last_value.of_bool tf) t.last_value)
       then if tf then rise t else fall t);
    t.last_value <- Last_value.of_bool tf;
    right t
  ;;
end

module Bit = struct
  type t =
    { canvas : Canvas.t
    ; env : Env.t
    ; value_column : El.t
    ; wave_row : El.t Wave_row.t
    ; data : Hardcaml_waveterm.Expert.Data.t
    }
  [@@deriving fields]

  let redraw (t : t) =
    Renderer_utils.clear_canvas t.env (C2d.get_context t.canvas);
    let env = t.env in
    let data = t.data in
    let canvas = t.canvas in
    let ctx = C2d.get_context canvas in
    C2d.set_stroke_style ctx (C2d.color (Jstr.v "black"));
    let value_at_selected_cycle =
      Renderer_utils.wave_data_get_opt data env.selected_cycle
    in
    let path_builder =
      Path_builder.create
        ~x:Constants.x_offset_to_start_of_signal
        ~y:Constants.y_offset_to_start_of_binary_signal
        ~half_cycle_width:env.half_cycle_width
        ~signal_height:env.binary_signal_height
    in
    let num_cycles_to_render = Env.num_cycles_to_render env in
    for i = 0 to num_cycles_to_render - 1 do
      Path_builder.step
        path_builder
        (Bits.to_bool (Hardcaml_waveterm.Expert.Data.get data (env.starting_cycle + i)));
      Path_builder.right path_builder
    done;
    El.set_children
      t.value_column
      [ El.txt'
          (match value_at_selected_cycle with
           | None -> ""
           | Some b -> if Bits.to_bool b then "1" else "0")
      ];
    C2d.set_line_width ctx Constants.signal_line_width;
    C2d.stroke ctx (Path_builder.path path_builder);
    Renderer_utils.draw_selected_cycle env canvas
  ;;

  let create (env : Env.t) ~update_view ~name ~data =
    let canvas = Renderer_utils.create_wave_canvas env in
    let canvas_el = Canvas.to_el canvas in
    El.set_inline_style
      (Jstr.of_string "height")
      (Jstr.of_string (sprintf "%fpx" (Env.canvas_height_in_pixels env)))
      canvas_el;
    El.set_inline_style
      (Jstr.of_string "width")
      (Jstr.of_string (sprintf "%fpx" (Env.canvas_width_in_pixels env)))
      canvas_el;
    let signal_column =
      Renderer_utils.signal_column
        [ El.txt (Jstr.of_string (Bytes.to_string (Bytes.of_string name))) ]
    in
    let value_column = Renderer_utils.value_column [] in
    El.set_inline_style (Jstr.v "font-family") (Jstr.v "\"Courier New\"") signal_column;
    El.set_inline_style (Jstr.v "font-family") (Jstr.v "\"Courier New\"") value_column;
    Renderer_utils.update_current_cycle_on_click ~canvas_el ~update_view ~env;
    let t =
      { canvas
      ; env
      ; wave_row =
          { signal_column
          ; value_column
          ; wave_column = Renderer_utils.wave_column [ canvas_el ]
          }
      ; value_column
      ; data
      }
    in
    redraw t;
    t
  ;;
end

module Clock = struct
  type t =
    { wave_row : El.t Wave_row.t
    ; env : Env.t
    ; canvas : Canvas.t
    }

  let wave_row (t : t) = t.wave_row

  let redraw t =
    Renderer_utils.clear_canvas t.env (C2d.get_context t.canvas);
    let env = t.env in
    let canvas = t.canvas in
    let ctx = C2d.get_context canvas in
    let path_builder =
      Path_builder.create
        ~x:Constants.x_offset_to_start_of_signal
        ~y:Constants.y_offset_to_start_of_binary_signal
        ~half_cycle_width:env.half_cycle_width
        ~signal_height:env.binary_signal_height
    in
    for _ = 0 to Env.num_cycles_to_render env - 1 do
      Path_builder.step path_builder true;
      Path_builder.step path_builder false
    done;
    C2d.set_stroke_style ctx (C2d.color (Jstr.v "black"));
    C2d.set_line_width ctx Constants.signal_line_width;
    C2d.stroke ctx (Path_builder.path path_builder);
    Renderer_utils.draw_selected_cycle env canvas
  ;;

  let create (env : Env.t) ~update_view ~name =
    let canvas = Renderer_utils.create_wave_canvas env in
    let canvas_el = Canvas.to_el canvas in
    El.set_inline_style
      (Jstr.of_string "height")
      (Jstr.of_string (sprintf "%fpx" (Env.canvas_height_in_pixels env)))
      canvas_el;
    El.set_inline_style
      (Jstr.of_string "width")
      (Jstr.of_string (sprintf "%fpx" (Env.canvas_width_in_pixels env)))
      canvas_el;
    let signal_column =
      Renderer_utils.signal_column
        [ El.txt (Jstr.of_string (Bytes.to_string (Bytes.of_string name))) ]
    in
    let value_column = Renderer_utils.value_column [] in
    El.set_inline_style (Jstr.v "font-family") (Jstr.v "\"Courier New\"") signal_column;
    El.set_inline_style (Jstr.v "font-family") (Jstr.v "\"Courier New\"") value_column;
    Renderer_utils.update_current_cycle_on_click ~canvas_el ~update_view ~env;
    { wave_row =
        { signal_column
        ; value_column
        ; wave_column = Renderer_utils.wave_column [ canvas_el ]
        }
    ; env
    ; canvas
    }
  ;;
end
