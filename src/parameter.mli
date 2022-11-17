open Base

module Typ : sig
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

val flag : t -> bool option
val int : t -> int option
val string : t -> string option
val symbol : t -> int option
