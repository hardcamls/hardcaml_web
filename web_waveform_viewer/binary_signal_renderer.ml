open Brr
open Brr_canvas
module Bits = Hardcaml.Bits

module Path_builder : sig
  type t

  val create : x:float -> y:float -> half_cycle_width:int -> signal_height:int -> t
  val rise : t -> unit
  val right : t -> unit
  val fall : t -> unit
  val step : t -> bool -> unit
  val path : t -> C2d.Path.t
end = struct
  type t =
    { path : C2d.Path.t
    ; signal_height : int
    ; half_cycle_width : int
    ; mutable x : float
    ; mutable y : float
    ; mutable last_value : bool
    }
  [@@deriving fields]

  let create ~x ~y ~half_cycle_width ~signal_height =
    let path = C2d.Path.create () in
    C2d.Path.move_to path ~x ~y;
    { path; x; y; last_value = false; half_cycle_width; signal_height }
  ;;

  let line_to (t : t) ~dx ~dy =
    let x = t.x +. Float.of_int dx in
    let y = t.y +. Float.of_int dy in
    C2d.Path.line_to t.path ~x ~y;
    t.x <- x;
    t.y <- y
  ;;

  let rise t = line_to t ~dx:0 ~dy:t.signal_height
  let right t = line_to t ~dx:t.half_cycle_width ~dy:0
  let fall t = line_to t ~dx:0 ~dy:(Int.neg t.signal_height)

  let step (t : t) tf =
    if tf <> t.last_value then if tf then rise t else fall t;
    t.last_value <- tf;
    right t
  ;;
end

let render_helper (env : Env.t) ~name ~f =
  let canvas = Canvas.create ~w:env.canvas_width ~h:env.canvas_height [] in
  let ctx = C2d.get_context canvas in
  C2d.set_font ctx (Jstr.of_string "120px Roboto");
  f ctx;
  let canvas_el = Canvas.to_el canvas in
  El.set_inline_style (Jstr.of_string "height") (Jstr.of_string "50px") canvas_el;
  El.set_inline_style (Jstr.of_string "width") (Jstr.of_string "1000px") canvas_el;
  El.tr
    [ El.td [ El.txt (Jstr.of_string (Bytes.to_string (Bytes.of_string name))) ]
    ; El.td [ canvas_el ]
    ]
;;

let render_clock (env : Env.t) ~name =
  render_helper env ~name ~f:(fun ctx ->
    let path_builder =
      Path_builder.create
        ~x:2.0
        ~y:2.0
        ~half_cycle_width:env.half_cycle_width
        ~signal_height:env.signal_height
    in
    for _ = 0 to Env.num_cycles_to_render env - 1 do
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
  render_helper env ~name ~f:(fun ctx ->
    let path_builder =
      Path_builder.create
        ~x:2.0
        ~y:2.0
        ~half_cycle_width:env.half_cycle_width
        ~signal_height:env.signal_height
    in
    let num_cycles_to_render =
      Int.min
        (Hardcaml_waveterm.Expert.Data.length data - env.starting_cycle)
        (Env.num_cycles_to_render env)
    in
    for i = 0 to num_cycles_to_render - 1 do
      Path_builder.step
        path_builder
        (Bits.to_bool (Hardcaml_waveterm.Expert.Data.get data (env.starting_cycle + i)));
      Path_builder.right path_builder
    done;
    C2d.set_line_width ctx 10.0;
    C2d.stroke ctx (Path_builder.path path_builder)
    (* draw_current_cycle ctx *))
;;
