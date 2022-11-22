open Base

type count =
  { count : int
  ; total : int
  ; max : int
  }

type t =
  { adders : count option
  ; subtractors : count option
  ; unsigned_multipliers : count option
  ; signed_multipliers : count option
  ; and_gates : count option
  ; or_gates : count option
  ; xor_gates : count option
  ; not_gates : count option
  ; equals : count option
  ; comparators : count option
  ; multiplexers : count option
  ; registers : count option
  ; memories : count option
  ; constants : count option
  ; wires : count option
  ; concatenation : count option
  ; part_selects : count option
  }

let create (u : Hardcaml.Circuit_utilization.t) =
  let tb (tb : Hardcaml.Circuit_utilization.Total_bits.t option) =
    Option.map tb ~f:(fun { count; total_bits } -> { count; total = total_bits; max = 0 })
  in
  let tm (tm : Hardcaml.Circuit_utilization.Total_and_max_bits.t option) =
    Option.map tm ~f:(fun { count; total_bits; max_instance_bits } ->
      { count; total = total_bits; max = max_instance_bits })
  in
  let mux_max_depth (m : Hardcaml.Circuit_utilization.Multiplexers.Mux_map.t) =
    Map.fold m ~init:0 ~f:(fun ~key ~data:_ m -> max m key)
  in
  let mem_max_depth (m : Hardcaml.Circuit_utilization.Memories.Mem_map.t) =
    Map.fold m ~init:0 ~f:(fun ~key ~data:_ m -> max m key.depth)
  in
  { adders = tm u.adders
  ; subtractors = tm u.subtractors
  ; unsigned_multipliers = tm u.unsigned_multipliers
  ; signed_multipliers = tm u.signed_multipliers
  ; and_gates = tb u.and_gates
  ; or_gates = tb u.or_gates
  ; xor_gates = tb u.xor_gates
  ; not_gates = tb u.not_gates
  ; equals = tm u.equals
  ; comparators = tm u.comparators
  ; multiplexers =
      Option.map u.multiplexers ~f:(fun { count; total_bits; multiplexers } ->
        { count; total = total_bits; max = mux_max_depth multiplexers })
  ; registers = tb u.registers
  ; memories =
      Option.map u.memories ~f:(fun { count; total_bits; memories } ->
        { count; total = total_bits; max = mem_max_depth memories })
  ; constants = tb u.constants
  ; wires = tb u.wires
  ; concatenation = tb u.concatenation
  ; part_selects = tb u.part_selects
  }
;;
