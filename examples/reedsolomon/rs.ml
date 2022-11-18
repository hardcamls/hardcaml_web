open! Base
open! Hardcaml
open Hardcaml_web

let title = "Hardcaml Reed-solomon Decoder"
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

  module Decoder = Codec.Decoder (struct
    let n = parallelism
  end)

  module I = Decoder.I
  module O = Decoder.O

  let create = Decoder.create
  let testbench = None
end
