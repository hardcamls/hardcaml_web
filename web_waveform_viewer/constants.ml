let canvas_scaling_factor = 2
let signal_line_width = Float.of_int (1 * canvas_scaling_factor)
let font_size_in_pixels = 12 * canvas_scaling_factor
let initial_half_cycle_width = 30 * canvas_scaling_factor
let initial_value_box_height = 30 * canvas_scaling_factor
let initial_binary_signal_height = 20 * canvas_scaling_factor
let initial_canvas_width = 1000 * canvas_scaling_factor
let initial_canvas_height = 50 * canvas_scaling_factor
let x_offset_to_start_of_signal = 2.0
let y_offset_to_start_of_value_box = 6.86 *. Float.of_int canvas_scaling_factor

let y_offset_to_start_of_binary_signal =
  y_offset_to_start_of_value_box
  +. Float.of_int ((initial_value_box_height - initial_binary_signal_height) / 2)
;;

let wave_colour = Jstr.v "green"
