open Core
open Brr_canvas
open Hardcaml

module Position = struct
  type t =
    { x : float
    ; y : float
    }
end

type t =
  { mutable last_value : Bits.t option
  ; mutable starting_position : Position.t
  ; mutable current_cycles : int
  ; context : C2d.t
  }

let create ~x ~y context =
  { last_value = None; starting_position = { x; y }; current_cycles = 0; context }
;;

let bits_to_hexstring b = Constant.to_hex_string ~signedness:Unsigned (Bits.to_constant b)
let width_per_half_cycle = 10
let width_per_cycle = 2 * width_per_half_cycle
let signal_height = 30

let render_last_value t =
  Option.iter t.last_value ~f:(fun last_value ->
    let context = t.context in
    let hexstring = "0x" ^ bits_to_hexstring last_value in
    (* Draw the rectangle *)
    C2d.stroke_rect
      context
      ~x:t.starting_position.x
      ~y:t.starting_position.y
      ~w:(Float.of_int (t.current_cycles * width_per_cycle))
      ~h:(Float.of_int signal_height);
    (* Render the text *)
    C2d.fill_text
      t.context
      (Jstr.of_string hexstring)
      ~x:(t.starting_position.x +. 10.0)
      ~y:(t.starting_position.y +. 20.0));
  t.starting_position
    <- { x = t.starting_position.x +. Float.of_int (t.current_cycles * width_per_cycle)
       ; y = t.starting_position.y
       };
  t.current_cycles <- 0;
  t.last_value <- None
;;

let step_with_value t current_value =
  let should_render_last_value =
    match t.last_value with
    | None -> false
    | Some last_value -> not (Bits.equal last_value current_value)
  in
  if should_render_last_value then render_last_value t else ();
  t.last_value <- Some current_value;
  t.current_cycles <- t.current_cycles + 1
;;
