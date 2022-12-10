open! Base
open Hardcaml
open! Hardcaml_waveterm
open Hardcaml_web

module Design = struct
  let top_level_name = "rac"

  let default_parameters =
    Parameter.
      [ "accumulator_bits", { typ = Int 16; description = "accumulator with" }
      ; "data_bits", { typ = Int 8; description = "input data width" }
      ; "int_coef_bits", { typ = Int 4; description = "integer coefficient bits" }
      ; "frac_coef_bits", { typ = Int 4; description = "fractional coefficient bits" }
      ]
  ;;

  module Make (P : Parameters.S) = struct
    let accumulator_bits = Parameters.as_int_exn P.parameters "accumulator_bits"
    let data_bits = Parameters.as_int_exn P.parameters "data_bits"
    let int_coef_bits = Parameters.as_int_exn P.parameters "int_coef_bits"
    let frac_coef_bits = Parameters.as_int_exn P.parameters "frac_coef_bits"
    let coef_prec = int_coef_bits + frac_coef_bits
    let num_coefs = 4

    module Rac = Hardcaml_circuits.Rac.Make (struct
      let mode = Hardcaml_circuits.Rac.Mode.Fixed
      let accumulator_bits = accumulator_bits
      let data_bits = data_bits
      let num_coefs = num_coefs
      let rom_shift = 0
    end)

    module I = Rac.I
    module O = Rac.O

    let create _scope ~build_mode:_ (i : _ I.t) =
      let coefs = Array.init num_coefs ~f:(fun _ -> Random.float 7.9 -. 3.95) in
      let scale = Float.(2. ** Float.of_int coef_prec) in
      let coefs =
        Array.map coefs ~f:(fun c ->
          Float.round_nearest_half_to_even (c *. scale)
          |> Float.to_int
          |> Bits.of_int ~width:frac_coef_bits)
      in
      Rac.create ~coefs i
    ;;

    let testbench (sim : (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t) =
      let inputs = Cyclesim.inputs sim in
      let waves, sim = Waveform.create sim in
      let data = Array.init num_coefs ~f:(fun _ -> Bits.random ~width:data_bits) in
      inputs.clr := Bits.vdd;
      Cyclesim.cycle sim;
      inputs.clr := Bits.gnd;
      for i = 0 to num_coefs - 1 do
        inputs.x.(i) := data.(i)
      done;
      for i = 0 to data_bits - 1 do
        inputs.en := Bits.vdd;
        if i = 0 then inputs.ld := Bits.vdd;
        if i = data_bits - 1 then inputs.addsub := Bits.vdd;
        Cyclesim.cycle sim;
        inputs.en := Bits.gnd;
        inputs.ld := Bits.gnd;
        inputs.addsub := Bits.gnd
      done;
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

let () = App.run ~javascript:"prefix.bc.js" ()
