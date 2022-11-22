open! Base

module Make (Design : Design.S) : sig
  val run_worker : unit -> unit Fut.t
end
