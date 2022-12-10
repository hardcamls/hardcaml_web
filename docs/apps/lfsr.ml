open! Base
open Hardcaml
open! Hardcaml_waveterm
open Hardcaml_web

module Design = struct
  let top_level_name = "lfsr"

  let default_parameters =
    Parameter.
      [ "data_width", { typ = Int 8; description = "lfsr width" }
      ; ( "type"
        , { typ = Symbol { options = [ "galois"; "fibonacci" ]; value = 0 }
          ; description = "calculation method"
          } )
      ; ( "gate"
        , { typ = Symbol { options = [ "xor"; "xnor" ]; value = 0 }
          ; description = "basic gate"
          } )
      ; ( "counterpart"
        , { typ = Flag false; description = "use counterpart tap configuration" } )
      ]
  ;;

  module Make (P : Parameters.S) = struct
    let data_width = Parameters.as_int_exn P.parameters "data_width"

    let config =
      let n = Parameters.as_symbol_exn P.parameters "type" in
      List.nth_exn Hardcaml_circuits.Lfsr.Config.all n.value
    ;;

    let op =
      let n = Parameters.as_symbol_exn P.parameters "gate" in
      List.nth_exn Hardcaml_circuits.Lfsr.Op.all n.value
    ;;

    let counterpart_taps = Parameters.as_flag_exn P.parameters "counterpart"

    module I = struct
      type 'a t = { d : 'a [@bits data_width] } [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { next : 'a [@bits data_width] } [@@deriving sexp_of, hardcaml]
    end

    let create _scope ~build_mode:_ (i : _ I.t) =
      { O.next =
          Hardcaml_circuits.Lfsr.create
            ~config
            ~counterpart_taps
            ~op
            (module Hardcaml.Signal)
            i.d
      }
    ;;

    let testbench (sim : (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t) =
      let inputs = Cyclesim.inputs sim in
      let outputs = Cyclesim.outputs sim in
      let waves, sim = Waveform.create sim in
      let steps = min 1000 (1 lsl data_width) in
      (* regardless of the lfsr type, it's always safe to start at 1. *)
      inputs.d := Bits.of_int ~width:data_width 1;
      for _ = 1 to steps do
        Cyclesim.cycle sim;
        inputs.d := !(outputs.next)
      done;
      let rules =
        let module I = Display_rules.With_interface (I) in
        let module O = Display_rules.With_interface (O) in
        [ I.default ~wave_format:Binary (); O.default ~wave_format:Binary () ]
        |> List.concat
      in
      Testbench_result.of_waves
        ~options:
          { display_width = 100; display_height = 10; start_cycle = 0; wave_width = 2 }
        ~rules
        waves
    ;;

    let testbench = Some testbench
  end
end

module App = Hardcaml_web.App.Make (Design)

let () = App.run ~javascript:"lfsr.bc.js" ()
