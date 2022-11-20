open! Base
open Brr
open Fut.Syntax

module Make (Design : Design.S) = struct
  let post m = Brr_webworkers.Worker.G.post m
  let status s = post (Messages.Worker_to_app.Status (Bytes.of_string s))

  let circuit parameters =
    status "Instantiating design";
    let module D =
      Design.Make (struct
        let parameters = parameters
      end)
    in
    let module Circuit = Hardcaml.Circuit.With_interface (D.I) (D.O) in
    let scope = Hardcaml.Scope.create ~flatten_design:true () in
    status "Generating circuit";
    scope, Circuit.create_exn ~name:Design.top_level_name (D.create scope)
  ;;

  let utilization parameters =
    let _, circuit = circuit parameters in
    status "Counting utilization";
    Hardcaml.Circuit_utilization.create circuit
  ;;

  let rtl parameters =
    let buffer = Buffer.create 1024 in
    let scope, circuit = circuit parameters in
    status "Generating RTL";
    Hardcaml.Rtl.output
      ~database:(Hardcaml.Scope.circuit_database scope)
      ~output_mode:(To_buffer buffer)
      Verilog
      circuit;
    Buffer.contents_bytes buffer
  ;;

  let simulation parameters =
    let module D =
      Design.Make (struct
        let parameters = parameters
      end)
    in
    match D.testbench with
    | None -> None
    | Some testbench ->
      status "Running simulation";
      let result = testbench () in
      Some result
  ;;

  let rec run_worker () =
    let recv_from_app e =
      let msg : Messages.App_to_worker.t = Brr_io.Message.Ev.data (Ev.as_type e) in
      match msg with
      | Utilization parameters ->
        (* XXX Apparently, not all fields in a utilization.t can be cloned?
           Either Map.t or recursive types? *)
        post (Messages.Worker_to_app.Utilization (utilization parameters))
      | Rtl parameters -> post (Messages.Worker_to_app.Rtl (rtl parameters))
      | Simulation parameters ->
        post (Messages.Worker_to_app.Simulation (simulation parameters))
    in
    if not (Brr_webworkers.Worker.ami ())
    then raise_s [%message "This should be run as a worker!"];
    let e = Ev.next Brr_io.Message.Ev.message G.target in
    let* _ = Fut.map recv_from_app e in
    run_worker ()
  ;;

  let () = ignore (run_worker ())
end
