open! Core
open Brr
open Brr_canvas
open Fut.Syntax

(* Build a small api for parameter input. *)

(* webworkers for simulation, circuit generation etc *)

(* download a file *)

(* Waveforms!!!! *)

(* Selecting testbenches.  Can we make them interactive? *)

let stripe_cnv_rect c ~x ~y ~w ~h =
  let x = Caml.truncate x
  and y = Caml.truncate y in
  let w = Caml.truncate w
  and h = Caml.truncate h in
  let idata = C2d.get_image_data c ~x ~y ~w ~h in
  let d = C2d.Image_data.data idata in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      if x mod 4 <> 0
      then ()
      else (
        let off = 4 * ((y * w) + x) in
        Tarray.set d off 0xFF;
        Tarray.set d (off + 1) 0x00;
        Tarray.set d (off + 2) 0x00)
    done
  done;
  C2d.put_image_data c idata ~x ~y
;;

let draw_brr c ~x ~y =
  let size = Caml.truncate (96. *. Window.device_pixel_ratio G.window) in
  C2d.set_font c Jstr.(v "bold " + of_int size + v "px SourceSansPro");
  C2d.fill_text c (Jstr.v " Brr!") ~x ~y
;;

let draw_rect c ~x ~y ~w ~h =
  C2d.set_fill_style c (C2d.color (Jstr.v "#000"));
  C2d.fill_rect c ~x ~y ~w ~h
;;

let draw cnv =
  let ctx = C2d.get_context cnv in
  let w = float @@ Canvas.w cnv in
  let h = float @@ Canvas.h cnv in
  printf "%f x %f\n" w h;
  C2d.stroke_rect ctx ~x:0. ~y:0. ~w ~h;
  let w = 0.5 *. w
  and h = 0.5 *. h in
  let x = w
  and y = h in
  draw_rect ctx ~x ~y ~w ~h;
  stripe_cnv_rect ctx ~x ~y ~w ~h;
  draw_brr ctx ~x:10. ~y:h
;;

let main () =
  let div = Document.find_el_by_id G.document (Jstr.v "webapp") in
  let inp = El.input () in
  let div_bits = El.div [] in
  let div_inp = El.div [ inp ] in
  let canvas = Canvas.create [] in
  Canvas.set_w canvas 400;
  Canvas.set_h canvas 400;
  let div_canvas = El.div [ Canvas.to_el canvas ] in
  let on_change _ =
    let vl = El.prop El.Prop.value inp in
    El.set_children
      div_bits
      El.[ txt' (Hardcaml.Bits.to_string (Hardcaml.Bits.of_string (Jstr.to_string vl))) ]
  in
  ignore (Ev.listen Ev.change on_change (El.as_target inp) : _);
  Option.iter div ~f:(fun div -> El.set_children div [ div_inp; div_bits; div_canvas ]);
  let* _ = Ev.next Ev.load (Window.as_target G.window) in
  if false then draw canvas;
  Fut.return ()
;;

let () = ignore (main ())
