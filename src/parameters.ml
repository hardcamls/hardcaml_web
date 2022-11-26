open Base

type t = (String.t, Parameter.t) List.Assoc.t [@@deriving sexp_of]

module type S = sig
  val parameters : t
end

let find t name = List.Assoc.find t name ~equal:String.equal

let find_map t name ~f =
  let open Option.Let_syntax in
  let%bind r = find t name in
  let%bind r = f r in
  return r
;;

let as_int t name = find_map t name ~f:Parameter.int
let as_int_exn t name = as_int t name |> Option.value_exn
let as_float t name = find_map t name ~f:Parameter.float
let as_float_exn t name = as_float t name |> Option.value_exn
let as_string t name = find_map t name ~f:Parameter.string
let as_string_exn t name = as_string t name |> Option.value_exn
let as_flag t name = find_map t name ~f:Parameter.flag
let as_flag_exn t name = as_flag t name |> Option.value_exn
let as_symbol t name = find_map t name ~f:Parameter.symbol
let as_symbol_exn t name = as_symbol t name |> Option.value_exn
