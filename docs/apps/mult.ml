open! Base
open Hardcaml
open! Hardcaml_waveterm
open Hardcaml_web

module Design = struct
  let top_level_name = "tree_multiplier"

  let default_parameters =
    Parameter.
      [ "data_width", { typ = Int 8; description = "argument width" }
      ; ( "style"
        , { typ = Symbol { options = [ "dadda"; "wallace" ]; value = 0 }
          ; description = "multiplier style"
          } )
      ]
  ;;

  module Make (P : Parameters.S) = struct
    let data_width = Parameters.as_int_exn P.parameters "data_width"

    let style =
      let n = Parameters.as_symbol_exn P.parameters "style" in
      List.nth_exn Hardcaml_circuits.Mul.Config.all n.value
    ;;

    module I = struct
      type 'a t =
        { a : 'a [@bits data_width]
        ; b : 'a [@bits data_width]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { c : 'a [@bits data_width * 2] } [@@deriving sexp_of, hardcaml]
    end

    let create _scope ~build_mode:_ (i : _ I.t) =
      { O.c = Hardcaml_circuits.Mul.create ~config:style (module Signal) i.a i.b }
    ;;

    let testbench (sim : (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t) =
      let inputs = Cyclesim.inputs sim in
      let waves, sim = Waveform.create sim in
      for _ = 1 to 10 do
        inputs.a := Bits.random ~width:data_width;
        inputs.b := Bits.random ~width:data_width;
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
          { display_width = 100; display_height = 12; start_cycle = 0; wave_width = 2 }
        ~rules
        waves
    ;;

    let testbench = Some testbench
  end
end

module App = Hardcaml_web.App.Make (Design)

let () = App.run ~javascript:"mult.bc.js" ()
