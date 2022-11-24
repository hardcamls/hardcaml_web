open Base

module Typ : sig
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

val flag : t -> bool option
val flag_exn : t -> bool
val int : t -> int option
val int_exn : t -> int
val string : t -> string option
val string_exn : t -> string
val symbol : t -> Typ.symbol option
val symbol_exn : t -> Typ.symbol
