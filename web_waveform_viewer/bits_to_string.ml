open Hardcaml

let hex bits = "0x" ^ Constant.to_hex_string ~signedness:Unsigned (Bits.to_constant bits)
let unsigned_int bits = Int.to_string (Bits.to_int bits)
let signed_int bits = Int.to_string (Bits.to_sint bits)
let binary bits = "0b" ^ Bits.to_string bits
