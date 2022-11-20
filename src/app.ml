open! Base
open Brr
open Fut.Syntax

module Make (Design : Design.S) = struct
  let background colour =
    At.v (Jstr.v "style") (Jstr.v ("background:#" ^ Printf.sprintf "%.6x" colour))
  ;;

  module App_divs = struct
    type t =
      { parameters : El.t
      ; control : El.t
      ; utilization : El.t
      ; ports : El.t
      ; simulation : El.t
      ; rtl : El.t
      }

    let create parameter_table =
      let parameters = El.div ~at:[ background 0xf8f8f8 ] [ parameter_table ] in
      let control = El.div [] in
      let utilization = El.div ~at:[ background 0xf0f0f0 ] [] in
      let ports = El.div ~at:[ background 0xe8e8e8 ] [] in
      let simulation = El.div [] in
      let rtl = El.div ~at:[ background 0xe0e0e0 ] [] in
      { parameters; control; utilization; ports; simulation; rtl }
    ;;

    let all { parameters; control; utilization; ports; simulation; rtl } =
      [ parameters; control; utilization; ports; simulation; rtl ]
    ;;
  end

  module Control_buttons = struct
    type t =
      { utilization : El.t
      ; ports : El.t
      ; simulation : El.t
      ; rtl : El.t
      }

    let create () =
      let utilization = El.button [ El.txt' "Utilization" ] in
      let ports = El.button [ El.txt' "Ports" ] in
      let simulation = El.button [ El.txt' "Simulate" ] in
      let rtl = El.button [ El.txt' "Rtl" ] in
      { utilization; ports; simulation; rtl }
    ;;

    let all { utilization; ports; simulation; rtl } =
      [ utilization; ports; simulation; rtl ]
    ;;

    let listen_and_post worker button msg =
      Ev.listen
        Ev.click
        (fun _ -> Brr_webworkers.Worker.post worker (msg ()))
        (El.as_target button)
    ;;
  end

  (* Input element for numbers *)
  let int_input (default : Parameter.t) =
    let e = El.input () in
    let v () = El.prop El.Prop.value e |> Jstr.to_int |> Option.value_exn in
    El.set_at (Jstr.v "type") (Some (Jstr.v "number")) e;
    El.set_at
      (Jstr.v "value")
      (Some (Jstr.v (Int.to_string (Parameter.int_exn default))))
      e;
    e, fun () -> { default with typ = Int (v ()) }
  ;;

  (* Input element for strings *)
  let string_input (default : Parameter.t) =
    let e = El.input () in
    let v () = El.prop El.Prop.value e |> Jstr.to_string in
    El.set_at (Jstr.v "type") (Some (Jstr.v "text")) e;
    El.set_at (Jstr.v "value") (Some (Jstr.v (Parameter.string_exn default))) e;
    e, fun () -> { default with typ = String (v ()) }
  ;;

  let flag_input (default : Parameter.t) =
    let e = El.input () in
    El.set_at (Jstr.v "type") (Some (Jstr.v "checkbox")) e;
    El.set_prop El.Prop.checked (Parameter.flag_exn default) e;
    e, fun () -> { default with typ = Flag (El.prop El.Prop.checked e) }
  ;;

  (* Create a table for the parameter, and return and accessor function. *)
  let parameters () =
    let parameters =
      List.map Design.default_parameters ~f:(fun (name, ({ typ; description } as p)) ->
        let value_, get =
          match typ with
          | String _ -> string_input p
          | Int _ -> int_input p
          | Flag _ -> flag_input p
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
    let tdc txt =
      let at = [ At.v (Jstr.v "style") (Jstr.v "text-align:center") ] in
      let t = El.td ~at [ txt ] in
      t
    in
    let port (name, width) =
      El.tr (List.map [ El.txt' name; El.txt (Jstr.of_int width) ] ~f:(fun d -> tdc d))
    in
    let inputs = List.map (D.I.to_list D.I.t) ~f:port in
    let outputs = List.map (D.O.to_list D.O.t) ~f:port in
    let table =
      let span2 = At.v (Jstr.v "colspan") (Jstr.of_int 2) in
      El.table
        ([ [ El.tr [ El.th ~at:[ span2 ] [ El.txt' "inputs" ] ] ]
         ; [ El.tr [ El.th [ El.txt' "name" ]; El.th [ El.txt' "width" ] ] ]
         ; inputs
         ; [ El.tr [ El.th ~at:[ span2 ] [ El.txt' "outputs" ] ] ]
         ; [ El.tr [ El.th [ El.txt' "name" ]; El.th [ El.txt' "width" ] ] ]
         ; outputs
         ]
        |> List.concat)
    in
    El.set_children div [ table ]
  ;;

  let table_of_utilization div (u : Hardcaml.Circuit_utilization.t) =
    let tdc x =
      let at = [ At.v (Jstr.v "style") (Jstr.v "text-align:center") ] in
      let t = El.td ~at [ El.txt (Jstr.of_int x) ] in
      t
    in
    let tdn = El.td [] in
    let tr f op x = El.tr (El.th [ El.txt' op ] :: f x) in
    let total_and_max_bits
      ({ count; total_bits; max_instance_bits } :
        Hardcaml.Circuit_utilization.Total_and_max_bits.t)
      =
      [ tdc count; tdc total_bits; tdc max_instance_bits ]
    in
    let total_bits ({ count; total_bits } : Hardcaml.Circuit_utilization.Total_bits.t) =
      [ tdc count; tdc total_bits; tdn ]
    in
    let muxes
      ({ count; total_bits; multiplexers = _ } :
        Hardcaml.Circuit_utilization.Multiplexers.t)
      =
      [ tdc count; tdc total_bits; tdn ]
    in
    let mems
      ({ count; total_bits; memories = _ } : Hardcaml.Circuit_utilization.Memories.t)
      =
      [ tdc count; tdc total_bits; tdn ]
    in
    let els =
      List.filter_opt
        [ Some
            (El.tr
               [ El.th [ El.txt' "op" ]
               ; El.th [ El.txt' "count" ]
               ; El.th [ El.txt' "total bits" ]
               ; El.th [ El.txt' "max bits" ]
               ])
        ; Option.map u.adders ~f:(tr total_and_max_bits "adders")
        ; Option.map u.subtractors ~f:(tr total_and_max_bits "subtractors")
        ; Option.map u.unsigned_multipliers ~f:(tr total_and_max_bits "unsigned mult")
        ; Option.map u.signed_multipliers ~f:(tr total_and_max_bits "signed mult")
        ; Option.map u.and_gates ~f:(tr total_bits "and gates")
        ; Option.map u.or_gates ~f:(tr total_bits "or gates")
        ; Option.map u.xor_gates ~f:(tr total_bits "xor gates")
        ; Option.map u.not_gates ~f:(tr total_bits "not gates")
        ; Option.map u.equals ~f:(tr total_and_max_bits "equality")
        ; Option.map u.comparators ~f:(tr total_and_max_bits "comparators")
        ; Option.map u.multiplexers ~f:(tr muxes "multiplexors")
        ; Option.map u.registers ~f:(tr total_bits "registers")
        ; Option.map u.memories ~f:(tr mems "memoriess")
        ; Option.map u.constants ~f:(tr total_bits "constants")
        ; Option.map u.wires ~f:(tr total_bits "wires")
        ; Option.map u.part_selects ~f:(tr total_bits "part selects")
        ]
    in
    El.set_children div [ El.table els ]
  ;;

  let testbench_result div (result : Testbench_result.t) =
    let waves =
      Option.map result.waves ~f:(fun { waves; options; rules } ->
        let display_width = Option.map options ~f:(fun o -> o.display_width) in
        let display_height = Option.map options ~f:(fun o -> o.display_height) in
        let start_cycle = Option.map options ~f:(fun o -> o.start_cycle) in
        let wave_width = Option.map options ~f:(fun o -> o.wave_width) in
        let display_rules = rules in
        El.div
          [ El.pre
              [ El.txt'
                  (Hardcaml_waveterm.Waveform.to_buffer
                     ?display_width
                     ?display_height
                     ?wave_width
                     ?start_cycle
                     ?display_rules
                     waves
                  |> Buffer.contents)
              ]
          ])
    in
    let result =
      Option.map result.result ~f:(function
        | Text t -> El.div [ El.txt' t ]
        | Brr_el el -> El.div [ el ])
    in
    El.set_children div (List.filter_opt [ waves; result ])
  ;;

  let rec process_messages_from_worker (divs : App_divs.t) worker =
    let recv_from_worker e =
      let (msg : Messages.Worker_to_app.t) = Brr_io.Message.Ev.data (Ev.as_type e) in
      match msg with
      | Utilization u -> table_of_utilization divs.utilization u
      | Rtl rtl -> El.set_children divs.rtl [ El.pre [ El.txt' (Bytes.to_string rtl) ] ]
      | Simulation result ->
        Option.iter result ~f:(fun result -> testbench_result divs.simulation result)
    in
    let msg =
      Ev.next Brr_io.Message.Ev.message (Brr_webworkers.Worker.as_target worker)
    in
    let* _ = Fut.map recv_from_worker msg in
    process_messages_from_worker divs worker
  ;;

  let run_app div_app worker =
    let parameters, parameter_table = parameters () in
    let divs = App_divs.create parameter_table in
    let buttons = Control_buttons.create () in
    Control_buttons.listen_and_post worker buttons.utilization (fun () ->
      Messages.App_to_worker.Utilization (parameters ()));
    Ev.listen Ev.click (generate_ports parameters divs.ports) (El.as_target buttons.ports);
    Control_buttons.listen_and_post worker buttons.simulation (fun () ->
      Messages.App_to_worker.Simulation (parameters ()));
    Control_buttons.listen_and_post worker buttons.rtl (fun () ->
      Messages.App_to_worker.Rtl (parameters ()));
    El.set_children div_app (El.h1 [ El.txt' Design.title ] :: App_divs.all divs);
    El.set_children divs.control (Control_buttons.all buttons);
    Fut.map (fun _ -> ()) (process_messages_from_worker divs worker)
  ;;

  let run div_id =
    (* Wait for document to fully load. *)
    let* _ = Ev.next Ev.load (Window.as_target G.window) in
    (* Start running the js code *)
    let div_app =
      match Document.find_el_by_id G.document (Jstr.v div_id) with
      | None -> raise_s [%message "Hardcaml application div was not found"]
      | Some div_app -> div_app
    in
    let worker =
      try Brr_webworkers.Worker.create (Jstr.v "hardcaml_worker.bc.js") with
      | _ -> raise_s [%message "Failed to create webworker."]
    in
    let* _ = run_app div_app worker in
    Fut.return ()
  ;;

  let () = ignore (run "hardcaml_app")
end
