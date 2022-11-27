open! Base
open Brr
open Fut.Syntax
module Bits = Hardcaml.Bits

module Make (Design : Design.S) = struct
  let background colour =
    At.v (Jstr.v "style") (Jstr.v ("background:#" ^ Printf.sprintf "%.6x" colour))
  ;;

  module App_divs = struct
    type t =
      { parameters : El.t
      ; control : El.t
      ; status : El.t
      ; utilization : El.t
      ; ports : El.t
      ; simulation : El.t
      ; rtl : El.t
      }

    let create parameter_table =
      let parameters = El.div ~at:[ background 0xf8f8f8 ] [ parameter_table ] in
      let control = El.div [] in
      let status = El.div [] in
      let utilization = El.div ~at:[ background 0xf0f0f0 ] [] in
      let ports = El.div ~at:[ background 0xe8e8e8 ] [] in
      let simulation = El.div [] in
      let rtl = El.div ~at:[ background 0xe0e0e0 ] [] in
      { parameters; control; status; utilization; ports; simulation; rtl }
    ;;

    let all { parameters; control; status; utilization; ports; simulation; rtl } =
      [ parameters; control; status; utilization; ports; simulation; rtl ]
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

    let disable_prop = El.Prop.bool (Jstr.v "disabled")
    let disable_all t = List.iter (all t) ~f:(fun e -> El.set_prop disable_prop true e)
    let enable_all t = List.iter (all t) ~f:(fun e -> El.set_prop disable_prop false e)

    let listen_and_post worker t button msg =
      Ev.listen
        Ev.click
        (fun _ ->
          Brr_webworkers.Worker.post worker (msg ());
          disable_all t)
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

  let float_input (default : Parameter.t) =
    let e = El.input () in
    let v () = El.prop El.Prop.value e |> Jstr.to_string |> Float.of_string in
    El.set_at (Jstr.v "type") (Some (Jstr.v "number")) e;
    El.set_at (Jstr.v "step") (Some (Jstr.v "0.1")) e;
    El.set_at
      (Jstr.v "value")
      (Some (Jstr.v (Printf.sprintf "%.1f" (Parameter.float_exn default))))
      e;
    e, fun () -> { default with typ = Float (v ()) }
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

  let symbol_input (default : Parameter.t) =
    let p = Parameter.symbol_exn default in
    let options =
      List.mapi p.options ~f:(fun idx o ->
        let value = At.value (Jstr.of_int idx) in
        let at = if idx = p.value then [ At.selected; value ] else [ value ] in
        El.option ~at [ El.txt' o ])
    in
    let e = El.select options in
    ( e
    , fun () ->
        let value =
          match
            List.findi options ~f:(fun _ o ->
              El.prop (El.Prop.bool (Jstr.v "selected")) o)
          with
          | Some (idx, _) -> idx
          | None -> 0 (* really, this is not possible, but lets not raise *)
        in
        { default with typ = Symbol { options = p.options; value } } )
  ;;

  (* Create a table for the parameter, and return and accessor function. *)
  let parameters () =
    let parameters =
      List.map Design.default_parameters ~f:(fun (name, ({ typ; description } as p)) ->
        let value_, get =
          match typ with
          | String _ -> string_input p
          | Int _ -> int_input p
          | Float _ -> float_input p
          | Flag _ -> flag_input p
          | Symbol _ -> symbol_input p
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

  let table_of_utilization div (u : Utilization.t) =
    let tdc x =
      let at = [ At.v (Jstr.v "style") (Jstr.v "text-align:center") ] in
      let t = El.td ~at [ El.txt (Jstr.of_int x) ] in
      t
    in
    let tdn = El.td [] in
    let tr op { Utilization.count; total; max } =
      El.tr
        [ El.th [ El.txt' op ]; tdc count; tdc total; (if max = 0 then tdn else tdc max) ]
    in
    let els =
      List.filter_opt
        [ Some
            (El.tr
               [ El.th [ El.txt' "op" ]
               ; El.th [ El.txt' "count" ]
               ; El.th [ El.txt' "total bits" ]
               ; El.th [ El.txt' "max width/depth" ]
               ])
        ; Option.map u.adders ~f:(tr "adders")
        ; Option.map u.subtractors ~f:(tr "subtractors")
        ; Option.map u.unsigned_multipliers ~f:(tr "unsigned mult")
        ; Option.map u.signed_multipliers ~f:(tr "signed mult")
        ; Option.map u.and_gates ~f:(tr "and gates")
        ; Option.map u.or_gates ~f:(tr "or gates")
        ; Option.map u.xor_gates ~f:(tr "xor gates")
        ; Option.map u.not_gates ~f:(tr "not gates")
        ; Option.map u.equals ~f:(tr "equality")
        ; Option.map u.comparators ~f:(tr "comparators")
        ; Option.map u.multiplexers ~f:(tr "multiplexors")
        ; Option.map u.registers ~f:(tr "registers")
        ; Option.map u.memories ~f:(tr "memoriess")
        ; Option.map u.constants ~f:(tr "constants")
        ; Option.map u.wires ~f:(tr "wires")
        ; Option.map u.part_selects ~f:(tr "part selects")
        ]
    in
    El.set_children div [ El.table els ]
  ;;

  let testbench_result div (result : Testbench_result.t) =
    let waves =
      Option.map result.waves ~f:(fun { waves; options; rules } ->
        let _display_width = Option.map options ~f:(fun o -> o.display_width) in
        let _display_height = Option.map options ~f:(fun o -> o.display_height) in
        let _start_cycle = Option.map options ~f:(fun o -> o.start_cycle) in
        let _wave_width = Option.map options ~f:(fun o -> o.wave_width) in
        let _display_rules = rules in
        El.div [ Hardcaml_web_waveform_viewer.render waves ])
    in
    let result =
      Option.map result.result ~f:(function
        | Text t -> El.div [ El.txt' t ]
        | Brr_el el -> El.div [ el ])
    in
    El.set_children div (List.filter_opt [ waves; result ])
  ;;

  let download ~filename ~contents =
    let element = El.a [] in
    El.set_at
      (Jstr.v "href")
      (Some
         (Jstr.( + )
            (Jstr.v "data:text/plain;charset=utf-8,")
            (match Brr.Uri.encode_component (Jstr.of_string contents) with
             | Ok x -> x
             | _ -> assert false)))
      element;
    El.set_at (Jstr.v "download") (Some (Jstr.v filename)) element;
    El.set_inline_style (Jstr.v "display") (Jstr.v "none") element;
    El.append_children (Document.body G.document) [ element ];
    El.click element;
    El.remove element
  ;;

  let rec process_messages_from_worker (divs : App_divs.t) buttons worker =
    let recv_from_worker e =
      let clear () =
        El.set_children divs.status [];
        Control_buttons.enable_all buttons
      in
      let (msg : Messages.Worker_to_app.t) = Brr_io.Message.Ev.data (Ev.as_type e) in
      match msg with
      | Utilization u ->
        table_of_utilization divs.utilization u;
        clear ()
      | Rtl rtl ->
        download
          ~filename:[%string "%{Design.top_level_name}.v"]
          ~contents:(Bytes.to_string rtl);
        clear ()
      | Simulation result ->
        Option.iter result ~f:(fun result -> testbench_result divs.simulation result);
        clear ()
      | Error (msg, parameters) ->
        El.set_children
          divs.status
          [ El.txt' (Bytes.to_string msg)
          ; El.txt' (Sexp.to_string_hum (Parameters.sexp_of_t parameters))
          ];
        Control_buttons.enable_all buttons
      | Status msg -> El.set_children divs.status [ El.txt' (Bytes.to_string msg) ]
    in
    let msg =
      Ev.next Brr_io.Message.Ev.message (Brr_webworkers.Worker.as_target worker)
    in
    let* _ = Fut.map recv_from_worker msg in
    process_messages_from_worker divs buttons worker
  ;;

  let run_app div_app worker =
    let parameters, parameter_table = parameters () in
    let divs = App_divs.create parameter_table in
    let buttons = Control_buttons.create () in
    Control_buttons.listen_and_post worker buttons buttons.utilization (fun () ->
      Messages.App_to_worker.Utilization (parameters ()));
    Ev.listen Ev.click (generate_ports parameters divs.ports) (El.as_target buttons.ports);
    Control_buttons.listen_and_post worker buttons buttons.simulation (fun () ->
      Messages.App_to_worker.Simulation (parameters ()));
    Control_buttons.listen_and_post worker buttons buttons.rtl (fun () ->
      Messages.App_to_worker.Rtl (parameters ()));
    El.set_children div_app (App_divs.all divs);
    El.set_children divs.control (Control_buttons.all buttons);
    Fut.map (fun _ -> ()) (process_messages_from_worker divs buttons worker)
  ;;

  let run ?(div = "hardcaml_app") ?(javascript = "hardcaml_app.bc.js") () =
    (* Wait for document to fully load. *)
    if Brr_webworkers.Worker.ami ()
    then
      let module Worker = Worker.Make (Design) in
      Worker.run_worker ()
    else
      let* _ = Ev.next Ev.load (Window.as_target G.window) in
      (* Start running the js code *)
      let div_app =
        match Document.find_el_by_id G.document (Jstr.v div) with
        | None -> raise_s [%message "Hardcaml application div was not found"]
        | Some div_app -> div_app
      in
      let worker =
        try Brr_webworkers.Worker.create (Jstr.v javascript) with
        | exn -> raise_s [%message "Failed to create webworker." (exn : exn)]
      in
      let* _ = run_app div_app worker in
      Fut.return ()
  ;;

  let run ?div ?javascript () = ignore (run ?div ?javascript ())
end
