open! Base
open! Hardcaml
open Hardcaml_waveterm
open Hardcaml_web

let top_level_name = "rsdecoder"

let default_parameters =
  Parameter.
    [ "n", { typ = Int 15; description = "Code word size" }
    ; "t", { typ = Int 2; description = "Parity" }
    ; "parallelism", { typ = Int 1; description = "Code word parallelism" }
    ]
;;

module Make (P : Parameters.S) = struct
  let n = Parameters.as_int_exn P.parameters "n"
  let t = Parameters.as_int_exn P.parameters "t"
  let parallelism = Parameters.as_int_exn P.parameters "parallelism"
  let k = n - (2 * t)
  let b = 0
  let m = Int.ceil_log2 (n + 1)

  module Gp = struct
    let pp =
      Array.foldi Reedsolomon.Galois.gf2_prim_polys.(m) ~init:0 ~f:(fun idx acc b ->
        acc + if b = 0 then 0 else b lsl idx)
    ;;

    let pe = 2
  end

  module Rp = struct
    let k = k
    let b = b
    let t = t
  end

  module Parallelism = struct
    let n = parallelism
  end

  module Codec =
    Hardcaml_reedsolomon.Codec.Make
      (struct
        let pp =
          Array.foldi Reedsolomon.Galois.gf2_prim_polys.(m) ~init:0 ~f:(fun idx acc b ->
            acc + if b = 0 then 0 else b lsl idx)
        ;;

        let pe = 2
      end)
      (struct
        let k = k
        let b = b
        let t = t
      end)

  open Test_hardcaml_reedsolomon
  module Standard = Reedsolomon.Standards.Make (Gp) (Rp)
  include Util.Make (Standard)
  module Decoder = Hw.Decoder (Parallelism)
  module Sim = Cyclesim.With_interface (Decoder.I) (Decoder.O)
  module I = Decoder.I
  module O = Decoder.O

  let create scope ~build_mode:_ i = Decoder.create scope i

  let rules =
    let module I = Display_rules.With_interface (Decoder.I) in
    let module O = Display_rules.With_interface (Decoder.O) in
    List.concat [ I.default (); O.default () ]
  ;;

  let testbench (sim : (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t) =
    let waves, sim = waveform_opt ~waves:true sim in
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs ~clock_edge:Before sim in
    let codeword = codeword (message ()) in
    let error = error 2 in
    let received = codeword ^. error in
    let cycles_per_codeword = (n + Parallelism.n - 1) / Parallelism.n in
    let offset = (cycles_per_codeword * Parallelism.n) - n in
    Cyclesim.reset sim;
    i.enable := Bits.vdd;
    i.clocking.clear := Bits.vdd;
    Cyclesim.cycle sim;
    i.clocking.clear := Bits.gnd;
    let recv = Array.concat [ rev received; Array.init offset ~f:(fun _ -> 0) ] in
    (* load received data *)
    i.first := Bits.vdd;
    i.load := Bits.vdd;
    for j = 0 to cycles_per_codeword - 1 do
      for k = 0 to Parallelism.n - 1 do
        i.x.(k) := Bits.of_int ~width:sbits recv.((j * Parallelism.n) + k)
      done;
      if j = cycles_per_codeword - 1 then i.last := Bits.vdd;
      Cyclesim.cycle sim;
      i.first := Bits.gnd;
      i.last := Bits.gnd
    done;
    i.load := Bits.gnd;
    let ocnt = ref 0 in
    let corrected = Array.init (cycles_per_codeword * Parallelism.n) ~f:(fun _ -> 0) in
    while !ocnt < cycles_per_codeword do
      Cyclesim.cycle sim;
      if Bits.to_int !(o.ordy) <> 0
      then (
        for k = 0 to Parallelism.n - 1 do
          corrected.((!ocnt * Parallelism.n) + k) <- Bits.to_int !(o.corrected.(k))
        done;
        Int.incr ocnt)
    done;
    let corrected = Sw.R.R.slice corrected (n - 1) in
    Cyclesim.cycle sim;
    if not ([%compare.equal: int array] corrected (rev codeword))
    then raise_s [%message (rev codeword : Sw.R.poly) (corrected : Sw.R.poly)];
    Testbench_result.of_waves
      ~options:
        { display_width = 160; display_height = 30; start_cycle = 0; wave_width = 0 }
      ~rules
      (Option.value_exn waves)
  ;;

  let testbench = Some testbench
end
