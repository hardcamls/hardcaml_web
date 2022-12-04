open Base

module App_to_worker = struct
  type t =
    | Utilization of Parameters.t
    | Rtl of
        { parameters : Parameters.t
        ; language : Hardcaml.Rtl.Language.t
        ; hierarchical_rtl : bool
        }
    | Simulation of Parameters.t
  [@@deriving sexp_of]
end

module Worker_to_app = struct
  type t =
    | Utilization of Utilization.t
    | Rtl of Bytes.t * Hardcaml.Rtl.Language.t
    | Simulation of Testbench_result.t option
    | Status of Bytes.t (* XXX neither string nor Jstr seemed to work. *)
    | Error of Bytes.t * Parameters.t
end
