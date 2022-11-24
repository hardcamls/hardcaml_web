open Base

type t = (String.t, Parameter.t) List.Assoc.t [@@deriving sexp_of]

module type S = sig
  val parameters : t
end

val find : t -> string -> Parameter.t option
val as_int : t -> string -> int option
val as_int_exn : t -> string -> int
val as_string : t -> string -> string option
val as_string_exn : t -> string -> string
val as_flag : t -> string -> bool option
val as_flag_exn : t -> string -> bool
val as_symbol : t -> string -> Parameter.Typ.symbol option
val as_symbol_exn : t -> string -> Parameter.Typ.symbol
