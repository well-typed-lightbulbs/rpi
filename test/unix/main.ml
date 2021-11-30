module Mtime = Rpi.Mtime.Make (Rpi_unix.Make (Rpi.Mtime))
module Clock = Rpi.Clock.Make (Mtime) (Rpi_unix.Make (Rpi.Clock))
module Gpio = Rpi.Gpio.Make (Rpi_unix.Make (Rpi.Gpio))
module Pwm = Rpi.Pwm.Make (Gpio) (Clock) (Mtime) (Rpi_unix.Make (Rpi.Pwm))

type color = { r : int; (* 8 bits *)
                        g : int; (* 8 bits *)
                                 b : int (* 8 bits *) }

let to_bits n =
  let rec aux acc n = function
    | 0 -> acc
    | i -> aux ((n mod 2 == 1) :: acc) (n lsr 1) (i - 1)
  in
  aux [] n 8

let color_to_bits { r; g; b } = to_bits g @ to_bits r @ to_bits b

let code b = if b then [ true; true; false ] else [ true; false; false ]

let color_to_code color = color_to_bits color |> List.map code |> List.flatten

let red = { r = 10; g = 0; b = 0 }

let green = { r = 0; g = 10; b = 0 }

let blue = { r = 0; g = 0; b = 10 }

let black = { r = 0; g = 0; b = 0 }

let white = { r = 1; g = 1; b = 1 }

let pattern_to_code pat = List.map color_to_code pat |> List.flatten

let group code =
  let rec aux acc current_number = function
    | [], l -> List.rev (Int32.shift_left current_number (32 - l) :: acc)
    | rest, 32 -> aux (current_number :: acc) 0l (rest, 0)
    | true :: rest, i ->
        aux acc
          (let ( * ) = Int32.mul in
           let ( + ) = Int32.add in
           (current_number * 2l) + 1l)
          (rest, Int.(i + 1))
    | false :: rest, i ->
        aux acc
          (let ( * ) = Int32.mul in
           current_number * 2l)
          (rest, Int.(i + 1))
  in
  aux [] 0l (code, 0)

let output data =
  Pwm.init ();
  for i = 0 to Array.length data - 1 do
    Pwm.write data.(i)
  done;
  Pwm.stop ()

let reset = List.init 61 (fun _ -> black)

let pattern =
  List.init 61 (fun i -> { r = i; g = (i + 10) mod 30; b = (i + 20) mod 30 })

let next = function first :: next -> next @ [ first ] | _ -> assert false

let stop = ref false

let _ = Sys.(signal sigint (Signal_handle (fun _ -> stop := true)))

let () =
  let rec loop pattern =
    output
      (pattern_to_code pattern |> group |> List.map Int32.to_int
     |> Array.of_list);
    Mtime.sleep_us 10_000L;
    if !stop then
      output
        (pattern_to_code reset |> group |> List.map Int32.to_int
       |> Array.of_list)
    else loop (next pattern)
  in
  loop pattern
