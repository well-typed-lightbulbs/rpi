

let () = 
Rpi.PWM.init;
let rec loop i=
Rpi.PWM.write i; loop (i+1) in
loop 0

(* let () = Printf.printf "Hello\n" *)
