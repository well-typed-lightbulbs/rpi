let pwm = Rpi.Pwm.base `Rpi4

let clock = Rpi.Clock.base `Rpi4

let gpio = Rpi.Gpio.base `Rpi4

let mtime = Rpi.Mtime.base `Rpi4

external sleep_ms : int -> unit = "caml_wait_msec" [@@noalloc]

(*
let () =
  Rpi.Pwm.init ~clock ~gpio pwm;
  let rec loop i =
    Rpi.Pwm.write pwm i;
    loop (i + 1)
  in
  loop 0
*)
let () =
  Printf.printf "Hello world !\n%!";
   Rpi.Gpio.set_func gpio P42 F_OUT;
  while true do
    sleep_ms 200_000;
    Rpi.Gpio.set gpio P42 true;
    sleep_ms 200_000;
    Rpi.Gpio.set gpio P42 false
  done
