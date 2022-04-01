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

let c = ref 0

let rec loop2 () =
  let* () = OS.Time.sleep_us 1000000L in
  let stat = Gc.quick_stat () in
  Printf.printf "%Ld: minor %d major %d compactions %d\n%!" 
    (OS.Time.now ())
    stat.minor_collections
    stat.major_collections
    stat.compactions;
  loop2 ()  

let rec echo () =
  let* c = Rpi.AUX.read () in
  Printf.printf "ECHO: '%c' %Ld\n%!" c (OS.Time.now ());
  echo ()

let program () = 
  Lwt.join [
    loop2 (); 
    echo ()
  ]


let () = 
  irq_enable ();
  Rpi.AUX.init ();
  OS.Hooks.register Rpi.AUX.restart_threads;
  OS.go (program ())
