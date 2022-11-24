open Base

module Typ = struct
  type symbol =
    { options : string list
    ; value : int
    }
  [@@deriving sexp_of]

  type t =
    | Flag of bool
    | Int of int
    | String of string
    | Symbol of symbol
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

let string_exn t = string t |> Option.value_exn

let int { typ; description = _ } =
  match typ with
  | Typ.Int i -> Some i
  | _ -> None
;;

let int_exn t = int t |> Option.value_exn

let flag { typ; description = _ } =
  match typ with
  | Typ.Flag f -> Some f
  | _ -> None
;;

let flag_exn t = flag t |> Option.value_exn

let symbol { typ; description = _ } =
  match typ with
  | Typ.Symbol value -> Some value
  | _ -> None
;;

let symbol_exn t = symbol t |> Option.value_exn
