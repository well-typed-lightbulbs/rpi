let pwm = Rpi.Pwm.base `Rpi4 |> Rpi.Pwm.map Rpi_unix.mmap

let clock = Rpi.Clock.base `Rpi4 |> Rpi.Clock.map Rpi_unix.mmap

let gpio = Rpi.Gpio.base `Rpi4 |> Rpi.Gpio.map Rpi_unix.mmap

let mtime = Rpi.Mtime.base `Rpi4 |> Rpi.Mtime.map Rpi_unix.mmap

let () = Printf.printf "Hello\n%!"


type color = {
  r: int; (* 8 bits *)
  g: int; (* 8 bits *)
  b: int; (* 8 bits *)
}

let to_bits n =
  let rec aux acc n = function
    | 0 -> acc
    | i -> aux ((n mod 2 == 1) :: acc) (n lsr 1) (i-1)
  in
  aux [] n 8

let color_to_bits {r;g;b} =
  to_bits g @ to_bits r @ to_bits b

let code b = 
  if b then [true; true; false] else [true; false; false]

let color_to_code color =
  color_to_bits color |> List.map code |> List.flatten


let red = {r=10; g=0;b=0}

let green = {r=0; g=10;b=0}

let blue = {r=0; g=0;b=10}

let black = {r=0; g=0; b=0}
let white = {r=1; g=1; b=1}

let pattern_to_code pat = List.map color_to_code pat |> List.flatten

let group code =
  let rec aux acc current_number = function
    | [], l -> List.rev ((Int32.shift_left current_number (32 - l)) :: acc)
    | rest, 32 -> aux (current_number :: acc) 0l (rest, 0)
    | true::rest, i -> aux acc (
        let ( * ) = Int32.mul in
        let ( + ) = Int32.add in
        current_number * 2l + 1l) (rest, Int.(i+1))
    | false::rest, i -> aux acc (
        let ( * ) = Int32.mul in
        current_number * 2l) (rest, Int.(i+1))
  in
  Printf.printf "Code: %d\n" (List.length code);
  aux [] 0l (code, 0)



let output data =
  Rpi.Pwm.init ~mtime ~clock ~gpio pwm;
  for i = 0 to Array.length data - 1 do
    Rpi.Pwm.write pwm  data.(i)
  done;
  Rpi.Pwm.stop pwm


let reset = List.init 60 (fun _ -> white) |> pattern_to_code |> group

let pattern = [red; blue; green ] @ (List.init 58 (fun _ -> white))

let next = function
  | first::next -> next @ [first]
  | _ -> assert false


let () =
  let rec loop pattern =
    output (pattern_to_code pattern |> group |> Array.of_list);
    Rpi.Mtime.sleep_us mtime 10000L;
    loop (next pattern)
  in
  loop (pattern)

(* 


module Peri = struct

  let base `Rpi4 = Mem.(Mmio.base + 0x20c000n)

  let registers_size = 0x28n


  module Make 
    (B: sig val base : Mem.addr end) = struct

    let blah = B.base 

  end


end

module Peri_bare_metal = Peri.Make(struct let base = Peri.base `Rpi4 end)

module Peri_unix = Peri.Make(struct 
let base = Rpi_unix.mmap ~size:Peri.registers_size (Peri.base `Rpi4) 
end)
 *)