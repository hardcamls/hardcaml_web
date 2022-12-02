module type S = sig
  type t

  val wave_row : t -> Brr.El.t Wave_row.t
  val resize : t -> unit
  val redraw : t -> unit
end

type t =
  | T :
      { impl : (module S with type t = 't)
      ; handle : 't
      }
      -> t

let resize (T { impl = (module Impl); handle }) = Impl.resize handle
let redraw (T { impl = (module Impl); handle }) = Impl.redraw handle
let wave_row (T { impl = (module Impl); handle }) = Impl.wave_row handle
