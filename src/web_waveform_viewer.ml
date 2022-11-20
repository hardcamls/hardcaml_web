open! Base
open Brr
open Brr_canvas
module Bits = Hardcaml.Bits

let render_clock ~name =
  let canvas = Canvas.create ~w:Constants.canvas_width ~h:Constants.canvas_height [] in
  let ctx = C2d.create canvas in
  let renderer = Binary_signal_renderer.create ~x:2.0 ~y:2.0 in
  C2d.set_font ctx (Jstr.of_string "12px Roboto");
  for _ = 0 to Constants.num_cycles_to_render - 1 do
    Binary_signal_renderer.rise renderer;
    Binary_signal_renderer.right renderer;
    Binary_signal_renderer.fall renderer;
    Binary_signal_renderer.right renderer
  done;
  Binary_signal_renderer.stroke ctx renderer;
  El.tr [ El.td [ El.txt' name ]; El.td [ Canvas.to_el canvas ] ]
;;

let render_bit ~(name : string) ~(data : Hardcaml_waveterm.Expert.Data.t) =
  let canvas = Canvas.create ~w:Constants.canvas_width ~h:Constants.canvas_height [] in
  let ctx = C2d.create canvas in
  let renderer = Binary_signal_renderer.create ~x:2.0 ~y:2.0 in
  C2d.set_font ctx (Jstr.of_string "12px Roboto");
  let num_cycles_to_render =
    Int.min (Hardcaml_waveterm.Expert.Data.length data) Constants.num_cycles_to_render
  in
  for i = 0 to num_cycles_to_render - 1 do
    Binary_signal_renderer.step
      renderer
      (Bits.to_bool (Hardcaml_waveterm.Expert.Data.get data i));
    Binary_signal_renderer.right renderer
  done;
  Binary_signal_renderer.stroke ctx renderer;
  El.tr [ El.td [ El.txt' name ]; El.td [ Canvas.to_el canvas ] ]
;;

let render_non_binary
  ~(name : string)
  ~(data : Hardcaml_waveterm.Expert.Data.t)
  ~(wave_format : Hardcaml_waveterm.Wave_format.t)
  =
  let canvas = Canvas.create ~w:Constants.canvas_width ~h:Constants.canvas_height [] in
  let ctx = C2d.create canvas in
  C2d.set_font ctx (Jstr.of_string "12px Roboto");
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
  let renderer = Non_binary_signal_renderer.create ~bits_to_string ~x:2.0 ~y:2.0 ctx in
  let num_cycles_to_render =
    Int.min (Hardcaml_waveterm.Expert.Data.length data) Constants.num_cycles_to_render
  in
  for i = 0 to num_cycles_to_render - 1 do
    let d = Hardcaml_waveterm.Expert.Data.get data i in
    Non_binary_signal_renderer.step renderer d
  done;
  Non_binary_signal_renderer.render_last_value renderer;
  El.tr [ El.td [ El.txt' name ]; El.td [ Canvas.to_el canvas ] ]
;;

let rec render_wave (wave : Hardcaml_waveterm.Expert.Wave.t) =
  match wave with
  | Clock name -> Some (render_clock ~name)
  | Binary (name, data) -> Some (render_bit ~name ~data)
  | Data (name, data, wave_format, _alignment) ->
    (match wave_format with
     | Bit -> Some (render_bit ~name ~data)
     | Bit_or wave_format ->
       if Hardcaml_waveterm.Expert.Data.length data > 0
          && Bits.width (Hardcaml_waveterm.Expert.Data.get data 0) = 1
       then Some (render_bit ~name ~data)
       else render_wave (Data (name, data, wave_format, _alignment))
     | Binary | Hex | Unsigned_int | Int | Index _ | Custom _ ->
       Some (render_non_binary ~name ~data ~wave_format))
  | Empty _ -> None
;;

let render (waveform : Hardcaml_waveterm.Waveform.t) =
  let open El in
  let waves = Hardcaml_waveterm.Waveform.waves waveform in
  let rows = Array.to_list waves |> List.filter_map ~f:render_wave in
  table [ thead [ th [ txt' "Signals" ]; th [ txt' "Waves" ] ]; tbody rows ]
;;
