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
    | Float of float
    | String of string
    | Symbol of symbol
  [@@deriving sexp_of]

  module type E = sig
    type t [@@deriving enumerate, sexp_of]
  end

  let of_enum (module E : E) ~value =
    Symbol
      { options =
          List.map E.all ~f:(fun e ->
            E.sexp_of_t e
            |> Sexp.to_string_hum
            |> String.map ~f:(fun c ->
                 match Char.lowercase c with
                 | '_' -> '-'
                 | c -> c))
      ; value
      }
  ;;
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

let float { typ; description = _ } =
  match typ with
  | Typ.Float i -> Some i
  | _ -> None
;;

let float_exn t = float t |> Option.value_exn

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
