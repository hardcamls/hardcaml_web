open! Base
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_web

let top_level_name = "fir_filter"

let default_parameters =
  Parameter.
    [ "data_width", { typ = Int 16; description = "input data width" }
    ; "num_taps", { typ = Int 4; description = "number of taps" }
    ; "coef_width", { typ = Int 8; description = "coefficient width" }
    ; "result_width", { typ = Int 32; description = "result width" }
    ; "pipeline", { typ = Flag true; description = "add pipeline stages to filter" }
    ]
;;

module Make (P : Parameters.S) = struct
  let data_width = Parameters.as_int_exn P.parameters "data_width"
  let coef_width = Parameters.as_int_exn P.parameters "coef_width"
  let result_width = Parameters.as_int_exn P.parameters "result_width"
  let num_taps = Parameters.as_int_exn P.parameters "num_taps"
  let piped = Parameters.as_flag_exn P.parameters "pipeline"

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; d : 'a [@bits data_width]
      ; coefficients : 'a list [@bits coef_width] [@length num_taps]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { q : 'a [@bits result_width] } [@@deriving sexp_of, hardcaml]
  end

  open Signal

  let create _scope (i : _ I.t) =
    let spec = Hardcaml.Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let piped d = if piped then reg spec d else d in
    let rec muls coefs d =
      match coefs with
      | [] -> []
      | coef :: coefs ->
        let d = reg spec d in
        let m = piped (d *+ coef) in
        sresize m result_width :: muls coefs d
    in
    let q =
      tree ~arity:2 (muls i.coefficients i.d) ~f:(fun d -> piped (reduce d ~f:( +: )))
    in
    { O.q }
  ;;

  let testbench () =
    let module Sim = Cyclesim.With_interface (I) (O) in
    let sim = Sim.create (create (Scope.create ())) in
    let inputs = Cyclesim.inputs sim in
    let waves, sim = Waveform.create sim in
    let coefs = List.init num_taps ~f:(fun _ -> Bits.random ~width:coef_width) in
    let data = List.init 10 ~f:(fun _ -> Bits.random ~width:data_width) in
    inputs.clear := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.clear := Bits.gnd;
    List.iter2_exn inputs.coefficients coefs ~f:( := );
    List.iter data ~f:(fun d ->
      inputs.d := d;
      Cyclesim.cycle sim);
    for _ = 0 to 5 do
      Cyclesim.cycle sim
    done;
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
