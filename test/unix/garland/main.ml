open Rpi

let light_pattern =
  let open Garland in
  let sp =
    speed 1.5
      (flip cyan --> pink --> red --> cyan --> pink
      ==> blue --> red --> cyan --> pink --> blue --> pink --> red)
  in
  sp === rev sp

let render ws =
  let time = Int64.to_float (Mtime.elapsed_us ()) /. 1_000_000. in
  Garland.render light_pattern ~nb_leds:300 time
  |> List.map (fun (r, g, b, _) ->
         let r = int_of_float r in
         let g = int_of_float g in
         let b = int_of_float b in
         { Ws2812b.r; g; b })
  |> Ws2812b.encode ws |> Ws2812b.output

let stop = ref false
let () = Sys.(set_signal sigint (Signal_handle (fun _ -> stop := true)))

let () =
  let ws = Ws2812b.init () in
  let reset_frame =
    List.init 361 (fun _ -> { Ws2812b.r = 0; g = 0; b = 0 })
    |> Ws2812b.encode ws
  in
  let rec loop () =
    let counter = Mtime.counter () in
    render ws;
    let offset = Mtime.counter_value_us counter in
    Mtime.sleep_us (Int64.sub 16_000L offset);
    (* 60 FPS *)
    if !stop then Ws2812b.output reset_frame else loop ()
  in
  loop ()
