open! Base
open Hardcaml
open! Hardcaml_waveterm
open Hardcaml_web

module Design = struct
  let top_level_name = "sorting_network"

  let default_parameters =
    Parameter.
      [ "data_width", { typ = Int 8; description = "data width" }
      ; "log_size", { typ = Int 2; description = "log network size" }
      ; ( "type"
        , { typ = Typ.of_enum (module Hardcaml_circuits.Sorting_network.Config) ~value:0
          ; description = "network type"
          } )
      ]
  ;;

  module Make (P : Parameters.S) = struct
    let data_width = Parameters.as_int_exn P.parameters "data_width"
    let log_size = Parameters.as_int_exn P.parameters "log_size"
    let size = 1 lsl max 1 log_size

    let network_type =
      let n = Parameters.as_symbol_exn P.parameters "type" in
      List.nth_exn Hardcaml_circuits.Sorting_network.Config.all n.value
    ;;

    module I = struct
      type 'a t = { d : 'a list [@bits data_width] [@length size] }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { q : 'a list [@bits data_width] [@length size] }
      [@@deriving sexp_of, hardcaml]
    end

    open Signal

    let compare_and_swap a b =
      let lt = a <: b in
      { Hardcaml_circuits.Sorting_network.Min_max.min = mux2 lt a b; max = mux2 lt b a }
    ;;

    let create _scope ~build_mode:_ (i : _ I.t) =
      { O.q = Hardcaml_circuits.Sorting_network.create network_type compare_and_swap i.d }
    ;;

    let testbench (sim : (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t) =
      let inputs = Cyclesim.inputs sim in
      let waves, sim = Waveform.create sim in
      for _ = 1 to 10 do
        List.iter inputs.d ~f:(fun d -> d := Bits.random ~width:data_width);
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
          { display_width = 100
          ; display_height = 5 + (2 * 3 * size)
          ; start_cycle = 0
          ; wave_width = 2
          }
        ~rules
        waves
    ;;

    let testbench = Some testbench
  end
end

module App = Hardcaml_web.App.Make (Design)

let () = App.run ~javascript:"sort.bc.js" ()
