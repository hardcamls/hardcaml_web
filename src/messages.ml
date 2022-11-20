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
    | Utilization of Hardcaml.Circuit_utilization.t
    | Rtl of Bytes.t
    | Simulation of Testbench_result.t option
end
