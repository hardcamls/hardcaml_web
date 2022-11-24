open! Base
open Hardcaml
open! Hardcaml_waveterm
open Hardcaml_web

let title = "no title yet"
let top_level_name = "no_name"
let default_parameters = []

module Make (P : Parameters.S) = struct
  module I = struct
    type 'a t = { d : 'a } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { q : 'a } [@@deriving sexp_of, hardcaml]
  end

  open Signal

  let create _scope (i : _ I.t) = { O.q = ~:(i.d) }

  let testbench (sim : (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t) =
    let inputs = Cyclesim.inputs sim in
    let waves, sim = Waveform.create sim in
    for i = 0 to 5 do
      inputs.d := Bits.of_bool (i % 2 = 0);
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
