open! Base
open Brr
open Fut.Syntax

module Make (Design : Design.S) = struct
  let printf = Stdio.printf

  (* Input element for numbers *)
  let int_input (default : Parameter.t) =
    let e = El.input () in
    let v () = El.prop El.Prop.value e |> Jstr.to_int |> Option.value_exn in
    El.set_at (Jstr.v "type") (Some (Jstr.v "number")) e;
    El.set_at
      (Jstr.v "value")
      (Some (Jstr.v (Int.to_string (Parameter.int default |> Option.value_exn))))
      e;
    e, fun () -> { default with typ = Int (v ()) }
  ;;

  (* Input element for strings *)
  let string_input (default : Parameter.t) =
    let e = El.input () in
    let v () = El.prop El.Prop.value e |> Jstr.to_string in
    El.set_at (Jstr.v "type") (Some (Jstr.v "text")) e;
    El.set_at
      (Jstr.v "value")
      (Some (Jstr.v (Parameter.string default |> Option.value_exn)))
      e;
    e, fun () -> { default with typ = String (v ()) }
  ;;

  (* Create a table for the parameter, and return and accessor function. *)
  let parameters () =
    let parameters =
      List.map Design.default_parameters ~f:(fun (name, ({ typ; description } as p)) ->
        let value_, get =
          match typ with
          | String _ -> string_input p
          | Int _ -> int_input p
          | Flag _ -> El.txt' "TODO flag", fun () -> p
          | Symbol _ -> El.txt' "TODO symbol", fun () -> p
        in
        let row = List.map [ El.txt' description; value_ ] ~f:(fun e -> El.td [ e ]) in
        name, El.tr row, get)
    in
    let get () = List.map parameters ~f:(fun (n, _, get) -> n, get ()) in
    let table = El.table (List.map parameters ~f:(fun (_, v, _) -> v)) in
    get, table
  ;;

  let table_of_ports (module I : Hardcaml.Interface.S) =
    List.map (I.to_list I.t) ~f:(fun (name, width) ->
      El.tr
        (List.map [ El.txt' name; El.txt (Jstr.of_int width) ] ~f:(fun d -> El.td [ d ])))
    |> El.table
  ;;

  let ports_of_interface (module I : Hardcaml.Interface.S) div =
    let ports = table_of_ports (module I) in
    El.set_children div [ ports ]
  ;;

  let generate_inputs parameters div _ =
    let module D =
      Design.Make (struct
        let parameters = parameters ()
      end)
    in
    ports_of_interface (module D.I) div
  ;;

  let generate_outputs parameters div _ =
    let module D =
      Design.Make (struct
        let parameters = parameters ()
      end)
    in
    ports_of_interface (module D.O) div
  ;;

  let generate_circuit parameters div _ =
    let module D =
      Design.Make (struct
        let parameters = parameters ()
      end)
    in
    let module Circuit = Hardcaml.Circuit.With_interface (D.I) (D.O) in
    let circ = Circuit.create_exn ~name:"design" D.create in
    let utilization = Hardcaml.Circuit_utilization.create circ in
    let results =
      Sexp.to_string_hum (Hardcaml.Circuit_utilization.sexp_of_t utilization)
    in
    El.set_children div [ El.txt' results ]
  ;;

  let simulate_circuit parameters div _ =
    let module D =
      Design.Make (struct
        let parameters = parameters ()
      end)
    in
    let waveform = Option.map D.testbench ~f:(fun testbench -> testbench ()) in
    Option.iter waveform ~f:(fun waveform ->
      let buf = Hardcaml_waveterm.Waveform.to_buffer waveform in
      El.set_children div [ El.pre [ El.txt' (Buffer.contents buf) ] ])
  ;;

  let run_app div_app =
    let parameters, parameter_table = parameters () in
    let div_parameters = El.div [ parameter_table ] in
    let div_utilization = El.div [] in
    let div_inputs = El.div [] in
    let div_outputs = El.div [] in
    let div_simulate = El.div [] in
    let generate_circuit_button = El.button [ El.txt' "Circuit" ] in
    let inputs_button = El.button [ El.txt' "Inputs" ] in
    let outputs_button = El.button [ El.txt' "Outputs" ] in
    let simulate_button = El.button [ El.txt' "Simulate" ] in
    let div_control =
      El.div [ generate_circuit_button; inputs_button; outputs_button; simulate_button ]
    in
    Ev.listen
      Ev.click
      (generate_circuit parameters div_utilization)
      (El.as_target generate_circuit_button);
    Ev.listen
      Ev.click
      (generate_inputs parameters div_inputs)
      (El.as_target inputs_button);
    Ev.listen
      Ev.click
      (generate_outputs parameters div_outputs)
      (El.as_target outputs_button);
    Ev.listen
      Ev.click
      (simulate_circuit parameters div_simulate)
      (El.as_target simulate_button);
    El.set_children
      div_app
      [ El.txt' "Welcome to the baby hardcaml application"
      ; div_parameters
      ; div_control
      ; div_utilization
      ; div_inputs
      ; div_outputs
      ; div_simulate
      ]
  ;;

  let run div_id =
    (* Wait for document to fully load. *)
    let* _ = Ev.next Ev.load (Window.as_target G.window) in
    (* Start running the js code *)
    (match Document.find_el_by_id G.document (Jstr.v div_id) with
     | None -> printf "Hardcaml application div was not found"
     | Some div_app -> run_app div_app);
    Fut.return ()
  ;;

  let () = ignore (run "hardcaml_app")
end
