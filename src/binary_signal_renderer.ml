open Brr_canvas

type t =
  { path : C2d.Path.t
  ; mutable x : float
  ; mutable y : float
  ; mutable last_value : bool
  }

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
let stroke ctx t = C2d.stroke ctx t.path

let step (t : t) tf =
  if tf <> t.last_value then if tf then rise t else fall t;
  t.last_value <- tf;
  right t
;;
