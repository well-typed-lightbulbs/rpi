let pwm = Rpi.Pwm.base `Rpi4

let clock = Rpi.Clock.base `Rpi4

let gpio = Rpi.Gpio.base `Rpi4

let () =
  Rpi.Pwm.init ~clock ~gpio pwm;
  let rec loop i =
    Rpi.Pwm.write pwm i;
    loop (i + 1)
  in
  loop 0

(* let () = Printf.printf "Hello\n" *)
