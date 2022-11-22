open Base

module App_to_worker = struct
  type t =
    | Utilization of Parameters.t
    | Rtl of Parameters.t
    | Simulation of Parameters.t
  [@@deriving sexp_of]
end

module Worker_to_app = struct
  type t =
    | Utilization of Utilization.t
    | Rtl of Bytes.t (* XXX a string led to a unicode error of some sort. *)
    | Simulation of Testbench_result.t option
    | Status of Bytes.t (* XXX neither string nor Jstr seemed to work. *)
end
