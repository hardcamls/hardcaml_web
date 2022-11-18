open Base

type parameters = (String.t, Parameter.t) List.Assoc.t

open Hardcaml

(** Parameter supplied by user. *)
module type Parameters = sig
  val parameters : parameters
end

module type S = sig
  (** Parameters expected by the design and it's testbench(es) *)
  val default_parameters : parameters

  (** Page title *)
  val title : string

  (** Top level module name *)
  val top_level_name : string

  module Make (P : Parameters) : sig
    module I : Interface.S
    module O : Interface.S

    (** Construct the design for the given parameters *)
    val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t

    (** Run a testbench (work out an appropriate return type...) *)
    val testbench : (unit -> Hardcaml_waveterm.Waveform.t) option
  end
end
