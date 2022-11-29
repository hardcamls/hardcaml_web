open! Brr

type 'a t =
  { signal_column : 'a
  ; value_column : 'a
  ; wave_column : 'a
  }
[@@deriving fields]

let map t ~f =
  { signal_column = f t.signal_column
  ; value_column = f t.value_column
  ; wave_column = f t.wave_column
  }
;;
