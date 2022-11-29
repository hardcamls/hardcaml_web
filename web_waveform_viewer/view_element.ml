module type S = sig
  type t

  val wave_row : t -> Wave_row.t
  val redraw : t -> unit
end

type t =
  | T :
      { impl : (module S with type t = 't)
      ; handle : 't
      }
      -> t

let redraw (T { impl = (module Impl); handle }) = Impl.redraw handle
let wave_row (T { impl = (module Impl); handle }) = Impl.wave_row handle
