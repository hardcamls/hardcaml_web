open Base
open Hardcaml

module type S = sig
  (** Parameters expected by the design and it's testbench(es) *)
  val default_parameters : Parameters.t

  (** Page title *)
  val title : string

  (** Top level module name *)
  val top_level_name : string

  module Make (P : Parameters.S) : sig
    module I : Interface.S
    module O : Interface.S

    (** Construct the design for the given parameters *)
    val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t

    (** Run a testbench (work out an appropriate return type...) *)
    val testbench : (unit -> Testbench_result.t) option
  end
end
