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

  let generate_ports parameters div _ =
    let module D =
      Design.Make (struct
        let parameters = parameters ()
      end)
    in
    let port (name, width) =
      El.tr
        (List.map [ El.txt' name; El.txt (Jstr.of_int width) ] ~f:(fun d -> El.td [ d ]))
    in
    let inputs = List.map (D.I.to_list D.I.t) ~f:port in
    let outputs = List.map (D.O.to_list D.O.t) ~f:port in
    let table =
      El.table
        (List.concat
           [ [ El.th [ El.txt' "inputs" ] ]
           ; inputs
           ; [ El.th [ El.txt' "outputs" ] ]
           ; outputs
           ])
    in
    El.set_children div [ table ]
  ;;

  let table_of_utilization (u : Hardcaml.Circuit_utilization.t) =
    let tr f op x = El.tr [ El.td [ El.txt' op ]; El.td [ El.txt' (f x) ] ] in
    let total_and_max_bits
      ({ count; total_bits; max_instance_bits } :
        Hardcaml.Circuit_utilization.Total_and_max_bits.t)
      =
      Printf.sprintf
        "count=%i, total bits=%i, max bits=%i"
        count
        total_bits
        max_instance_bits
    in
    let total_bits ({ count; total_bits } : Hardcaml.Circuit_utilization.Total_bits.t) =
      Printf.sprintf "count=%i, total bits=%i" count total_bits
    in
    let muxes
      ({ count; total_bits; multiplexers = _ } :
        Hardcaml.Circuit_utilization.Multiplexers.t)
      =
      Printf.sprintf "count=%i, total_bits=%i" count total_bits
    in
    let mems
      ({ count; total_bits; memories = _ } : Hardcaml.Circuit_utilization.Memories.t)
      =
      Printf.sprintf "count=%i, total_bits=%i" count total_bits
    in
    let els =
      List.filter_opt
        [ Option.map u.adders ~f:(tr total_and_max_bits "(+:)")
        ; Option.map u.subtractors ~f:(tr total_and_max_bits "(-:)")
        ; Option.map u.unsigned_multipliers ~f:(tr total_and_max_bits "(*:)")
        ; Option.map u.signed_multipliers ~f:(tr total_and_max_bits "(*+)")
        ; Option.map u.and_gates ~f:(tr total_bits "(&:)")
        ; Option.map u.or_gates ~f:(tr total_bits "(|:)")
        ; Option.map u.xor_gates ~f:(tr total_bits "(^:)")
        ; Option.map u.not_gates ~f:(tr total_bits "(~:)")
        ; Option.map u.equals ~f:(tr total_and_max_bits "(==:)")
        ; Option.map u.comparators ~f:(tr total_and_max_bits "(<:)")
        ; Option.map u.multiplexers ~f:(tr muxes "mux")
        ; Option.map u.registers ~f:(tr total_bits "reg")
        ; Option.map u.memories ~f:(tr mems "mems")
        ; Option.map u.constants ~f:(tr total_bits "const")
        ; Option.map u.wires ~f:(tr total_bits "wire")
        ; Option.map u.part_selects ~f:(tr total_bits "select")
        ]
    in
    El.table els
  ;;

  let generate_circuit_utilization parameters div _ =
    let module D =
      Design.Make (struct
        let parameters = parameters ()
      end)
    in
    let module Circuit = Hardcaml.Circuit.With_interface (D.I) (D.O) in
    let scope = Hardcaml.Scope.create ~flatten_design:true () in
    let circ = Circuit.create_exn ~name:Design.top_level_name (D.create scope) in
    let utilization = Hardcaml.Circuit_utilization.create circ in
    El.set_children div [ table_of_utilization utilization ]
  ;;

  let simulate_circuit parameters div _ =
    let module D =
      Design.Make (struct
        let parameters = parameters ()
      end)
    in
    let waveform = Option.map D.testbench ~f:(fun testbench -> testbench ()) in
    Option.iter waveform ~f:(fun waveform ->
      let buf =
        Hardcaml_waveterm.Waveform.to_buffer
          ~display_width:120
          ~display_height:35
          ~wave_width:2
          waveform
      in
      El.set_children div [ El.pre [ El.txt' (Buffer.contents buf) ] ])
  ;;

  let generate_rtl parameters div _ =
    let module D =
      Design.Make (struct
        let parameters = parameters ()
      end)
    in
    let module Circuit = Hardcaml.Circuit.With_interface (D.I) (D.O) in
    let scope = Hardcaml.Scope.create () in
    let circ = Circuit.create_exn ~name:Design.top_level_name (D.create scope) in
    let buffer = Buffer.create 1024 in
    Hardcaml.Rtl.output
      ~database:(Hardcaml.Scope.circuit_database scope)
      ~output_mode:(To_buffer buffer)
      Verilog
      circ;
    El.set_children div [ El.pre [ El.txt' (Buffer.contents buffer) ] ]
  ;;

  let run_app div_app =
    let parameters, parameter_table = parameters () in
    let div_parameters = El.div [ parameter_table ] in
    let div_utilization = El.div [] in
    let div_ports = El.div [] in
    let div_simulate = El.div [] in
    let div_rtl = El.div [] in
    let generate_circuit_utilization_button = El.button [ El.txt' "Utilization" ] in
    let ports_button = El.button [ El.txt' "Ports" ] in
    let simulate_button = El.button [ El.txt' "Simulate" ] in
    let rtl_button = El.button [ El.txt' "Rtl" ] in
    let div_control =
      El.div
        [ generate_circuit_utilization_button; ports_button; simulate_button; rtl_button ]
    in
    Ev.listen
      Ev.click
      (generate_circuit_utilization parameters div_utilization)
      (El.as_target generate_circuit_utilization_button);
    Ev.listen Ev.click (generate_ports parameters div_ports) (El.as_target ports_button);
    Ev.listen
      Ev.click
      (simulate_circuit parameters div_simulate)
      (El.as_target simulate_button);
    Ev.listen Ev.click (generate_rtl parameters div_rtl) (El.as_target rtl_button);
    El.set_children
      div_app
      [ El.h1 [ El.txt' Design.title ]
      ; div_parameters
      ; div_control
      ; div_utilization
      ; div_ports
      ; div_simulate
      ; div_rtl
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
