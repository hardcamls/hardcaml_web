open! Base
open Hardcaml
open! Hardcaml_waveterm
open Hardcaml_web

module Design = struct
  let top_level_name = "cordic"

  let default_parameters =
    Parameter.
      [ "int_prec", { typ = Int 3; description = "integer precision" }
      ; "frac_prec", { typ = Int 14; description = "fractional precision" }
      ; "iterations", { typ = Int 16; description = "number of iterations" }
      ; ( "mode"
        , { typ = Typ.of_enum (module Hardcaml_circuits.Cordic_reference.Mode) ~value:0
          ; description = "mode"
          } )
      ; ( "system"
        , { typ = Typ.of_enum (module Hardcaml_circuits.Cordic_reference.System) ~value:0
          ; description = "system"
          } )
      ; "c", { typ = Float 1.; description = "c" }
      ; "x", { typ = Float 1.; description = "x" }
      ; "y", { typ = Float 1.; description = "y" }
      ; "z", { typ = Float 1.; description = "z" }
      ]
  ;;

  module Make (P : Parameters.S) = struct
    let int_prec = Parameters.as_int_exn P.parameters "int_prec"
    let frac_prec = Parameters.as_int_exn P.parameters "frac_prec"
    let iterations = Parameters.as_int_exn P.parameters "iterations"
    let c = Parameters.as_float_exn P.parameters "c"
    let x = Parameters.as_float_exn P.parameters "x"
    let y = Parameters.as_float_exn P.parameters "y"
    let z = Parameters.as_float_exn P.parameters "z"

    let mode =
      let n = Parameters.as_symbol_exn P.parameters "mode" in
      List.nth_exn Hardcaml_circuits.Cordic_reference.Mode.all n.value
    ;;

    let system =
      let n = Parameters.as_symbol_exn P.parameters "system" in
      List.nth_exn Hardcaml_circuits.Cordic_reference.System.all n.value
    ;;

    let architecture = Hardcaml_circuits.Cordic.Architecture.Iterative

    module Fix = struct
      let width = int_prec + frac_prec
      let fractional_width = frac_prec
    end

    module Fixnum = Hardcaml_circuits.Fixnum.Make (Fix)
    module Cordic = Hardcaml_circuits.Cordic.Make (Fix)

    module I = struct
      let w = int_prec + frac_prec

      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; enable : 'a
        ; load : 'a
        ; c : 'a [@bits w]
        ; x : 'a [@bits w]
        ; y : 'a [@bits w]
        ; z : 'a [@bits w]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = Cordic.O

    let create _scope ~build_mode:_ (i : _ I.t) =
      Cordic.create
        { Hardcaml_circuits.Cordic.Config.architecture; iterations }
        { Cordic.I.clk = i.clock
        ; clr = i.clear
        ; enable = i.enable
        ; ld = i.load
        ; system = Hardcaml_circuits.Cordic.System.to_signal system
        ; mode = Hardcaml_circuits.Cordic.Mode.to_signal mode
        ; c = i.c
        ; x = i.x
        ; y = i.y
        ; z = i.z
        }
    ;;

    let testbench (sim : (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t) =
      let inputs = Cyclesim.inputs sim in
      let waves, sim = Waveform.create sim in
      let c = Fixnum.of_float c in
      let x = Fixnum.of_float x in
      let y = Fixnum.of_float y in
      let z = Fixnum.of_float z in
      inputs.clear := Bits.vdd;
      Cyclesim.cycle sim;
      inputs.clear := Bits.gnd;
      inputs.c := Fixnum.to_bits c;
      inputs.x := Fixnum.to_bits x;
      inputs.y := Fixnum.to_bits y;
      inputs.z := Fixnum.to_bits z;
      for i = 0 to iterations - 1 do
        inputs.enable := Bits.vdd;
        inputs.load := Bits.gnd;
        if i = 0 then inputs.load := Bits.vdd;
        Cyclesim.cycle sim
      done;
      inputs.enable := Bits.gnd;
      Cyclesim.cycle sim;
      Cyclesim.cycle sim;
      let rules =
        let module I = Display_rules.With_interface (I) in
        let module O = Display_rules.With_interface (O) in
        [ I.default ~wave_format:(Bit_or Int) (); O.default ~wave_format:(Bit_or Int) () ]
        |> List.concat
      in
      Testbench_result.of_waves
        ~options:
          { display_width = 100; display_height = 30; start_cycle = 0; wave_width = 2 }
        ~rules
        waves
    ;;

    let testbench = Some testbench
  end
end

module App = Hardcaml_web.App.Make (Design)

let () = App.run ~javascript:"cordic.bc.js" ()
