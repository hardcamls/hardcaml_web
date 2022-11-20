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

val create : Hardcaml.Circuit_utilization.t -> t
