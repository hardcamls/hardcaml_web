open Brr_canvas

type t =
  { path : C2d.Path.t
  ; mutable x : float
  ; mutable y : float
  }

let create ~x ~y =
  let path = C2d.Path.create () in
  C2d.Path.move_to path ~x ~y;
  { path; x; y }
;;

let line_to (t : t) ~dx ~dy =
  let x = t.x +. dx in
  let y = t.y +. dy in
  C2d.Path.line_to t.path ~x ~y;
  t.x <- x;
  t.y <- y
;;

let rise_and_stroke_right ~half_cycle_width ~binary_signal_height t =
  line_to t ~dx:0.0 ~dy:binary_signal_height;
  line_to t ~dx:half_cycle_width ~dy:0.0
;;

let fall_and_stroke_right ~half_cycle_width ~binary_signal_height t =
  line_to t ~dx:0.0 ~dy:(Float.neg binary_signal_height);
  line_to t ~dx:half_cycle_width ~dy:0.0
;;

let stroke ctx t = C2d.stroke ctx t.path
