open Base

module Typ = struct
  type t =
    | Flag of bool
    | Int of int
    | String of string
    | Symbol of
        { options : string array
        ; value : int
        }
  [@@deriving sexp_of]
end

type t =
  { typ : Typ.t
  ; description : string
  }
[@@deriving sexp_of]

let string { typ; description = _ } =
  match typ with
  | Typ.String s -> Some s
  | _ -> None
;;

let int { typ; description = _ } =
  match typ with
  | Typ.Int i -> Some i
  | _ -> None
;;

let flag { typ; description = _ } =
  match typ with
  | Typ.Flag f -> Some f
  | _ -> None
;;

let symbol { typ; description = _ } =
  match typ with
  | Typ.Symbol { value; _ } -> Some value
  | _ -> None
;;
