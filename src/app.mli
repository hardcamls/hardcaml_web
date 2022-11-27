open! Base

module Make (Design : Design.S) : sig
  val run
    :  ?div:string (** [div] elemenent to run application within *)
    -> ?javascript:string (** Name of javascript file *)
    -> unit
    -> unit
end
