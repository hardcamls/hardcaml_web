open! Base
open Brr
open Fut.Syntax

module Make (Design : Design.S) = struct
  let post m = Brr_webworkers.Worker.G.post m
  let status s = post (Messages.Worker_to_app.Status (Bytes.of_string s))

  let error s parameters =
    post (Messages.Worker_to_app.Error (Bytes.of_string s, parameters))
  ;;

  let circuit ~flatten_design ~build_mode parameters =
    status "Instantiating design";
    let module D =
      Design.Make (struct
        let parameters = parameters
      end)
    in
    let module Circuit = Hardcaml.Circuit.With_interface (D.I) (D.O) in
    let scope = Hardcaml.Scope.create ~flatten_design () in
    status "Generating circuit";
    scope, Circuit.create_exn ~name:Design.top_level_name (D.create scope ~build_mode)
  ;;

  let utilization parameters =
    let _, circuit = circuit ~flatten_design:true ~build_mode:Synthesis parameters in
    status "Counting utilization";
    let utilization = Hardcaml.Circuit_utilization.create circuit in
    Utilization.create utilization
  ;;

  let rtl parameters language hierarchical_rtl =
    let buffer = Buffer.create 1024 in
    let scope, circuit =
      circuit ~flatten_design:(not hierarchical_rtl) ~build_mode:Synthesis parameters
    in
    status "Generating RTL";
    Hardcaml.Rtl.output
      ~database:(Hardcaml.Scope.circuit_database scope)
      ~output_mode:(To_buffer buffer)
      language
      circuit;
    Buffer.contents_bytes buffer
  ;;

  let simulation parameters =
    let module D =
      Design.Make (struct
        let parameters = parameters
      end)
    in
    let module Sim = Hardcaml.Cyclesim.With_interface (D.I) (D.O) in
    match D.testbench with
    | None -> None
    | Some testbench ->
      let _, circuit = circuit ~flatten_design:true ~build_mode:Simulation parameters in
      status "Creating simulator";
      let sim = Hardcaml.Cyclesim.create circuit in
      let sim = Sim.coerce sim in
      status "Running simulation";
      let result = testbench sim in
      Some result
  ;;

  (* Make the API's safe-ish by catching all exceptions and reporting them.*)

  let utilization parameters =
    match utilization parameters with
    | utilization -> post (Messages.Worker_to_app.Utilization utilization)
    | exception e -> error (Exn.to_string e) parameters
  ;;

  let rtl parameters language hierarchical_rtl =
    match rtl parameters language hierarchical_rtl with
    | rtl -> post (Messages.Worker_to_app.Rtl (rtl, language))
    | exception e -> error (Exn.to_string e) parameters
  ;;

  let simulation parameters =
    match simulation parameters with
    | simulation -> post (Messages.Worker_to_app.Simulation simulation)
    | exception e -> error (Exn.to_string e) parameters
  ;;

  let rec run_worker () =
    let recv_from_app e =
      let msg : Messages.App_to_worker.t = Brr_io.Message.Ev.data (Ev.as_type e) in
      match msg with
      | Utilization parameters -> utilization parameters
      | Rtl { parameters; language; hierarchical_rtl } ->
        rtl parameters language hierarchical_rtl
      | Simulation parameters -> simulation parameters
    in
    if not (Brr_webworkers.Worker.ami ())
    then raise_s [%message "This should be run as a worker!"];
    let e = Ev.next Brr_io.Message.Ev.message G.target in
    let* _ = Fut.map recv_from_app e in
    run_worker ()
  ;;
end
