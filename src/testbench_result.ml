open Base
open Hardcaml_waveterm

type wave_options =
  { display_width : int
  ; display_height : int
  ; start_cycle : int
  ; wave_width : int
  }

type waves =
  { waves : Waveform.t
  ; options : wave_options Option.t
  ; rules : Display_rules.t Option.t
  }

type result =
  | Text of string
  | Brr_el of Brr.El.t

type t =
  { waves : waves Option.t
  ; result : result Option.t
  }

let none = { waves = None; result = None }

let of_waves ?options ?rules waves =
  { waves = Some { waves; options; rules }; result = None }
;;

let of_text text = { waves = None; result = Some (Text text) }
