let () =
  Rpi.Pwm.init ();
  let rec loop i =
    Rpi.Pwm.write i;
    loop (i + 1)
  in
  loop 0

(* let () = Printf.printf "Hello\n" *)
