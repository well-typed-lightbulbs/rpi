(*open Garland

let test5 =
  speed 1.5
    (flip cyan --> pink --> red --> cyan --> pink
    ==> blue --> red --> cyan --> pink --> blue --> pink --> red)

let test6 = test5 === rev test5 |> map_luminosity (( *. ) 0.15)

let rec loop () =
  let time = Rpi.Mtime.elapsed_us () in
  Garland.render ~nb_leds:361 test6 (Int64.to_float time /. 1_000_000.)
  |> List.map (fun (r, g, b, _) ->
         { Ws2812b.r = Float.to_int r; g = Float.to_int g; b = Float.to_int b })
  |> Ws2812b.encode |> Ws2812b.output;
  loop ()

let () = loop ()
*)


external irq_enable : unit -> unit = "irq_enable"
open Lwt.Syntax

let rec loop1 () =
  let* () = OS.Time.sleep_us 500000L in
  Printf.printf "Loop 1. %Ld\n%!" (OS.Time.now ());
  loop1 ()
  
let rec loop2 () =
  let* () = OS.Time.sleep_us 3000000L in
  Printf.printf "Loop 2. %Ld\n%!" (OS.Time.now ());
  loop2 ()  

let program () =
  Lwt.join [
    loop1 ();
    loop2 ();
  ]

  

let () = 
  irq_enable ();
  OS.go (program ())
