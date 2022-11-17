open! Base
open Hardcaml
open Hardcaml_web

let default_parameters =
  Parameter.
    [ "foo", { typ = String ""; description = "the foo" }
    ; "width", { typ = Int 1; description = "data width" }
    ]
;;

module Make (P : Design.Parameters) = struct
  let data_width =
    List.Assoc.find_exn ~equal:String.equal P.parameters "width"
    |> Parameter.int
    |> Option.value_exn
  ;;

  module I = struct
    type 'a t =
      { clock : 'a
      ; d : 'a [@bits data_width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { q : 'a [@bits data_width] } [@@deriving sexp_of, hardcaml]
  end

  let create (i : _ I.t) =
    let q = Hardcaml.Signal.reg (Hardcaml.Reg_spec.create ~clock:i.clock ()) i.d in
    { O.q }
  ;;

  let testbench () =
    let module Sim = Cyclesim.With_interface (I) (O) in
    let sim = Sim.create create in
    let waves, sim = Hardcaml_waveterm.Waveform.create sim in
    Cyclesim.reset sim;
    waves
  ;;

  let testbench = Some testbench
end
