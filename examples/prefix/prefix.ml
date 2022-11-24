open! Base
open Hardcaml
open! Hardcaml_waveterm
open Hardcaml_web

let top_level_name = "prefix_adder"

let default_parameters =
  Parameter.
    [ "data_width", { typ = Int 8; description = "input data width" }
    ; ( "network"
      , { typ =
            Symbol
              { options = [ "serial"; "sklansky"; "brent-kung"; "kooge-stone" ]
              ; value = 1
              }
        ; description = "type of network"
        } )
    ]
;;

module Make (P : Parameters.S) = struct
  let data_width = Parameters.as_int_exn P.parameters "data_width"

  let network =
    let n = Parameters.as_symbol_exn P.parameters "network" in
    List.nth_exn Hardcaml_circuits.Prefix_sum.Config.all n.value
  ;;

  module I = struct
    type 'a t =
      { a : 'a [@bits data_width]
      ; b : 'a [@bits data_width]
      ; c_in : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { c : 'a [@bits data_width + 1] } [@@deriving sexp_of, hardcaml]
  end

  module Sum = Hardcaml_circuits.Prefix_sum

  let create _scope (i : _ I.t) =
    let must_be_power_of_2 =
      match network with
      | Serial | Sklansky -> false
      | Brent_kung | Kogge_stone -> true
    in
    let w = Int.ceil_pow2 data_width in
    let a, b =
      if must_be_power_of_2 then Signal.uresize i.a w, Signal.uresize i.b w else i.a, i.b
    in
    let c =
      Hardcaml_circuits.Prefix_sum.create
        (module Signal)
        ~config:network
        ~input1:a
        ~input2:b
        ~carry_in:i.c_in
    in
    let c = Signal.sel_bottom c (data_width + 1) in
    { O.c }
  ;;

  let testbench () =
    let module Sim = Cyclesim.With_interface (I) (O) in
    let sim = Sim.create (create (Scope.create ())) in
    let inputs = Cyclesim.inputs sim in
    let waves, sim = Waveform.create sim in
    for _ = 0 to 10 do
      inputs.a := Bits.random ~width:data_width;
      inputs.b := Bits.random ~width:data_width;
      inputs.c_in := Bits.random ~width:1;
      Cyclesim.cycle sim
    done;
    let rules =
      let module I = Display_rules.With_interface (I) in
      let module O = Display_rules.With_interface (O) in
      [ I.default ~wave_format:(Bit_or Unsigned_int) ()
      ; O.default ~wave_format:(Bit_or Unsigned_int) ()
      ]
      |> List.concat
    in
    Testbench_result.of_waves
      ~options:
        { display_width = 100; display_height = 17; start_cycle = 0; wave_width = 2 }
      ~rules
      waves
  ;;

  let testbench = Some testbench
end
