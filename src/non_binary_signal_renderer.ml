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
  ; bits_to_string : Bits.t -> string
  }

let create ~x ~y ~bits_to_string context =
  { last_value = None
  ; starting_position = { x; y }
  ; current_cycles = 0
  ; context
  ; bits_to_string
  }
;;

let width_per_half_cycle = Float.to_int Constants.half_cycle_width
let width_per_cycle = 2 * width_per_half_cycle
let signal_height = Constants.signal_height

let create_value_to_render ~max_width_allowed ~value ~ctx ~bits_to_string =
  let can_fit x =
    let text_metric = C2d.measure_text ctx (Jstr.of_string x) in
    let width = C2d.Text_metrics.width text_metric in
    Float.O.(width <= Float.of_int max_width_allowed)
  in
  With_return.with_return (fun { return } ->
    let value = bits_to_string value in
    (* If the original text as it is works, then return it. *)
    if can_fit value then return (Some value);
    (* Otherwise, keep stripping a character and add some dots at the end until it fits. *)
    let rec loop text : unit =
      match text with
      | "" -> ()
      | _ ->
        let candidate = text ^ ".." in
        if can_fit candidate then return (Some candidate);
        loop (String.subo ~len:(String.length text - 1) text)
    in
    loop value;
    (* If nothing fits, try a dot. *)
    if can_fit "." then Some "." else None)
;;

let render_last_value t =
  Option.iter t.last_value ~f:(fun last_value ->
    let context = t.context in
    let string_to_render =
      create_value_to_render
        ~max_width_allowed:((t.current_cycles * width_per_cycle) - 3)
        ~value:last_value
        ~ctx:context
        ~bits_to_string:t.bits_to_string
    in
    (* Draw the rectangle *)
    C2d.stroke_rect
      context
      ~x:t.starting_position.x
      ~y:t.starting_position.y
      ~w:(Float.of_int (t.current_cycles * width_per_cycle))
      ~h:signal_height;
    (* Render the text *)
    Option.iter string_to_render ~f:(fun string_to_render ->
      C2d.fill_text
        t.context
        (Jstr.of_string string_to_render)
        ~x:(t.starting_position.x +. 5.0)
        ~y:(t.starting_position.y +. 20.0)));
  t.starting_position
    <- { x = t.starting_position.x +. Float.of_int (t.current_cycles * width_per_cycle)
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
