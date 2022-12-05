open Brr

let create_icon icon_name =
  El.button
    [ El.span
        ~at:
          [ At.class' (Jstr.of_string "material-icons")
          ; At.class' (Jstr.of_string "small")
          ]
        [ El.txt' icon_name ]
    ]
;;

let to_beginning_of_simulation () = create_icon "first_page"
let backwards_fast () = create_icon "arrow_back_ios"
let backwards_normal () = create_icon "chevron_left"
let forwards_normal () = create_icon "chevron_right"
let forwards_fast () = create_icon "arrow_forward_ios"
let to_end_of_simulation () = create_icon "last_page"
let zoom_in () = create_icon "zoom_in"
let zoom_out () = create_icon "zoom_out"
let zoom_out_map () = create_icon "zoom_out_map"

(* <input class="textbox" value="0"></input> *)
