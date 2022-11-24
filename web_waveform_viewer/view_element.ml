module type S = sig
  type t

  val el : t -> Brr.El.t
  val redraw : t -> unit
end

type t =
  | T :
      { impl : (module S with type t = 't)
      ; handle : 't
      }
      -> t

let redraw (T { impl = (module Impl); handle }) = Impl.redraw handle
let el (T { impl = (module Impl); handle }) = Impl.el handle
