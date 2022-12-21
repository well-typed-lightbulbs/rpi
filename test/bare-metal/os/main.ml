let rec main () =
  Printf.printf ">>\n%!";
  let open Lwt.Syntax in
  let* () = OS.Time.sleep_us 1_000_000L in
  Printf.printf "OK\n%!";
  main ()

let () =
  OS.go (main ())
