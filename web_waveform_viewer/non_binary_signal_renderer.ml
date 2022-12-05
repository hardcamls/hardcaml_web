open Core
open Brr
open Brr_canvas
open Hardcaml
module Text_alignment = Hardcaml_waveterm.Text_alignment

module Position = struct
  type t =
    { x : float
    ; y : float
    }
end

(* fyquah: Renderer is an odd name *)
module Renderer = struct
  type t =
    { mutable last_value : Bits.t option
    ; mutable starting_position : Position.t
    ; mutable current_cycles : int
    ; env : Env.t
    ; context : C2d.t
    ; bits_to_string : Bits.t -> string
    ; alignment : Text_alignment.t
    }

  let create ~x ~y ~bits_to_string ~env ~alignment context =
    { last_value = None
    ; starting_position = { x; y }
    ; current_cycles = 0
    ; context
    ; env
    ; bits_to_string
    ; alignment
    }
  ;;

  let width_per_cycle (t : t) = 2 * t.env.half_cycle_width

  let create_value_to_render
    ~max_width_allowed
    ~value
    ~ctx
    ~bits_to_string
    ~(alignment : Text_alignment.t)
    =
    With_return.with_return (fun { return } ->
      let value = bits_to_string value in
      let number_of_characters_that_can_fit =
        let font_width_in_pixels = 0.60 *. Float.of_int Constants.font_size_in_pixels in
        let max_width_allowed = Float.of_int max_width_allowed in
        Float.to_int (max_width_allowed /. font_width_in_pixels)
      in
      if String.length value <= number_of_characters_that_can_fit
      then Some value
      else if number_of_characters_that_can_fit <= 0
      then None
      else if number_of_characters_that_can_fit <= 1
      then Some "."
      else if number_of_characters_that_can_fit <= 2
      then Some ".."
      else (
        let num_chars_to_strip =
          String.length value - (number_of_characters_that_can_fit - 2)
          |> Int.max 0
          |> Int.min (String.length value)
        in
        Some
          (match alignment with
           | Left ->
             String.subo ~len:(String.length value - num_chars_to_strip) value ^ ".."
           | Right -> ".." ^ String.subo ~pos:num_chars_to_strip value)))
  ;;

  let render_last_value t =
    Option.iter t.last_value ~f:(fun last_value ->
      let context = t.context in
      let string_to_render =
        let max_width_allowed =
          (t.current_cycles * width_per_cycle t) - (5 * Constants.canvas_scaling_factor)
        in
        create_value_to_render
          ~max_width_allowed
          ~value:last_value
          ~ctx:context
          ~bits_to_string:t.bits_to_string
          ~alignment:t.alignment
      in
      (* Draw the rectangle *)
      C2d.stroke_rect
        context
        ~x:t.starting_position.x
        ~y:t.starting_position.y
        ~w:(Float.of_int (t.current_cycles * width_per_cycle t))
        ~h:(Float.of_int t.env.value_box_height);
      (* Render the text *)
      Option.iter string_to_render ~f:(fun string_to_render ->
        C2d.fill_text
          t.context
          (Jstr.of_string string_to_render)
          ~x:(t.starting_position.x +. Float.of_int (5 * Constants.canvas_scaling_factor))
          ~y:(t.starting_position.y +. Float.of_int (20 * Constants.canvas_scaling_factor))));
    t.starting_position
      <- { x = t.starting_position.x +. Float.of_int (t.current_cycles * width_per_cycle t)
         ; y = t.starting_position.y
         };
    t.current_cycles <- 0;
    t.last_value <- None
  ;;

  let step t current_value =
    let should_render_last_value =
      match t.last_value with
      | None -> false
      | Some last_value -> not (Bits.equal last_value current_value)
    in
    if should_render_last_value then render_last_value t else ();
    t.last_value <- Some current_value;
    t.current_cycles <- t.current_cycles + 1
  ;;
end

type t =
  { canvas : Canvas.t
  ; canvas_el : El.t
  ; value_column : El.t
  ; bits_to_string : Bits.t -> string
  ; env : Env.t
  ; wave_row : Brr.El.t Wave_row.t
  ; data : Hardcaml_waveterm.Expert.Data.t
  ; alignment : Text_alignment.t
  }

let wave_row t = t.wave_row

let resize (t : t) =
  Canvas.set_h t.canvas t.env.canvas_height;
  Canvas.set_w t.canvas t.env.canvas_width;
  El.set_inline_style
    (Jstr.of_string "height")
    (Jstr.of_string (sprintf "%fpx" (Env.canvas_height_in_pixels t.env)))
    t.canvas_el;
  El.set_inline_style
    (Jstr.of_string "width")
    (Jstr.of_string (sprintf "%fpx" (Env.canvas_width_in_pixels t.env)))
    t.canvas_el
;;

let redraw (t : t) =
  Renderer_utils.clear_canvas t.env (C2d.get_context t.canvas);
  let ctx = C2d.get_context t.canvas in
  C2d.set_line_width ctx Constants.signal_line_width;
  C2d.set_stroke_style ctx (C2d.color Constants.wave_colour);
  C2d.set_fill_style ctx (C2d.color Constants.wave_colour);
  C2d.set_font
    ctx
    (Jstr.of_string (sprintf "%dpx Courier New" Constants.font_size_in_pixels));
  let num_cycles_to_render =
    Int.min
      (Hardcaml_waveterm.Expert.Data.length t.data - t.env.starting_cycle)
      (Env.num_cycles_to_render t.env)
  in
  let renderer =
    Renderer.create
      ~bits_to_string:t.bits_to_string
      ~x:Constants.x_offset_to_start_of_signal
      ~y:Constants.y_offset_to_start_of_value_box
      ~env:t.env
      ~alignment:t.alignment
      ctx
  in
  for i = 0 to num_cycles_to_render - 1 do
    let d = Hardcaml_waveterm.Expert.Data.get t.data (t.env.starting_cycle + i) in
    Renderer.step renderer d
  done;
  Renderer.render_last_value renderer;
  Renderer_utils.draw_selected_cycle t.env t.canvas;
  El.set_children
    t.value_column
    [ El.txt'
        (match Renderer_utils.wave_data_get_opt t.data t.env.selected_cycle with
         | None -> ""
         | Some bits -> t.bits_to_string bits)
    ]
;;

let create
  ~(name : string)
  ~(data : Hardcaml_waveterm.Expert.Data.t)
  ~(wave_format : Hardcaml_waveterm.Wave_format.t)
  ~(alignment : Text_alignment.t)
  ~update_view
  (env : Env.t)
  =
  let canvas = Renderer_utils.create_wave_canvas env in
  let bits_to_string =
    match wave_format with
    | Hex -> Bits_to_string.hex
    | Unsigned_int -> Bits_to_string.unsigned_int
    | Int -> Bits_to_string.signed_int
    | Index l -> fun x -> List.nth_exn l (Bits.to_int x)
    | Custom f -> f
    | Binary ->
      (* XXX fyquah: This should be Bits_to_string.binary I think? *)
      Bits_to_string.hex
    | Bit | Bit_or _ -> (* Impossible. *) assert false
  in
  let value_column = Renderer_utils.value_column [] in
  let signal_column =
    Renderer_utils.signal_column [ El.txt' (Bytes.to_string (Bytes.of_string name)) ]
  in
  El.set_inline_style (Jstr.v "font-family") (Jstr.v "\"Courier New\"") signal_column;
  El.set_inline_style (Jstr.v "font-family") (Jstr.v "\"Courier New\"") value_column;
  let canvas_el = Canvas.to_el canvas in
  Renderer_utils.update_current_cycle_on_click ~canvas_el ~update_view ~env;
  let t =
    { canvas
    ; canvas_el
    ; value_column
    ; wave_row =
        { signal_column
        ; value_column
        ; wave_column = Renderer_utils.wave_column [ canvas_el ]
        }
    ; env
    ; bits_to_string
    ; data
    ; alignment
    }
  in
  redraw t;
  t
;;
