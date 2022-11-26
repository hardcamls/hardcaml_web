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
    | Float of float
    | String of string
    | Symbol of symbol
  [@@deriving sexp_of]

  module type E = sig
    type t [@@deriving enumerate, sexp_of]
  end

  val of_enum : (module E) -> value:int -> t
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
val float : t -> float option
val float_exn : t -> float
val string : t -> string option
val string_exn : t -> string
val symbol : t -> Typ.symbol option
val symbol_exn : t -> Typ.symbol
