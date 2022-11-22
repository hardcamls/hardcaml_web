open Brr
open Brr_canvas
module Bits = Hardcaml.Bits

module Path_builder : sig
  type t

  val create : x:float -> y:float -> t
  val rise : t -> unit
  val right : t -> unit
  val fall : t -> unit
  val step : t -> bool -> unit
  val path : t -> C2d.Path.t
end = struct
  type t =
    { path : C2d.Path.t
    ; mutable x : float
    ; mutable y : float
    ; mutable last_value : bool
    }
  [@@deriving fields]

  let create ~x ~y =
    let path = C2d.Path.create () in
    C2d.Path.move_to path ~x ~y;
    { path; x; y; last_value = false }
  ;;

  let line_to (t : t) ~dx ~dy =
    let x = t.x +. dx in
    let y = t.y +. dy in
    C2d.Path.line_to t.path ~x ~y;
    t.x <- x;
    t.y <- y
  ;;

  let rise t = line_to t ~dx:0.0 ~dy:Constants.signal_height
  let right t = line_to t ~dx:Constants.half_cycle_width ~dy:0.0
  let fall t = line_to t ~dx:0.0 ~dy:(Float.neg Constants.signal_height)

  let step (t : t) tf =
    if tf <> t.last_value then if tf then rise t else fall t;
    t.last_value <- tf;
    right t
  ;;
end

let render_helper ~name ~f =
  let canvas = Canvas.create ~w:Constants.canvas_width ~h:Constants.canvas_height [] in
  let ctx = C2d.get_context canvas in
  C2d.set_font ctx (Jstr.of_string "120px Roboto");
  f ctx;
  let canvas_el = Canvas.to_el canvas in
  El.set_inline_style (Jstr.of_string "height") (Jstr.of_string "50px") canvas_el;
  El.set_inline_style (Jstr.of_string "width") (Jstr.of_string "1000px") canvas_el;
  El.tr [ El.td [ El.txt' name ]; El.td [ canvas_el ] ]
;;

(* XXX fyquah: Some kind of Env.t to represent all kinds of current state? *)
let _draw_current_cycle ctx =
  (* Draw the cursor to indicate the current cycle *)
  C2d.set_stroke_style ctx (C2d.color (Jstr.of_string "blue"));
  C2d.set_line_width ctx 30.0;
  C2d.stroke
    ctx
    (let path = C2d.Path.create () in
     C2d.Path.move_to path ~x:2.0 ~y:0.0;
     C2d.Path.line_to path ~x:2.0 ~y:(Float.of_int Constants.canvas_height);
     path)
;;

let render_clock ~name =
  render_helper ~name ~f:(fun ctx ->
    let path_builder = Path_builder.create ~x:2.0 ~y:2.0 in
    for _ = 0 to Constants.num_cycles_to_render - 1 do
      Path_builder.rise path_builder;
      Path_builder.right path_builder;
      Path_builder.fall path_builder;
      Path_builder.right path_builder
    done;
    C2d.set_line_width ctx 10.0;
    C2d.stroke ctx (Path_builder.path path_builder)
    (* draw_current_cycle ctx *))
;;

let render_bit (env : Env.t) ~(name : string) ~(data : Hardcaml_waveterm.Expert.Data.t) =
  render_helper ~name ~f:(fun ctx ->
    let path_builder = Path_builder.create ~x:2.0 ~y:2.0 in
    let num_cycles_to_render =
      Int.min
        (Hardcaml_waveterm.Expert.Data.length data - env.current_cycle)
        Constants.num_cycles_to_render
    in
    for i = 0 to num_cycles_to_render - 1 do
      Path_builder.step
        path_builder
        (Bits.to_bool (Hardcaml_waveterm.Expert.Data.get data (env.current_cycle + i)));
      Path_builder.right path_builder
    done;
    C2d.set_line_width ctx 10.0;
    C2d.stroke ctx (Path_builder.path path_builder)
    (* draw_current_cycle ctx *))
;;
