module Pwm = Rpi.Pwm.Make (struct
  let mode = Rpi.Pwm.Analog
  let pins = [ Rpi.Pwm.Pin40; Rpi.Pwm.Pin41 ]
  let freq = 54 * 1_000_000 / 2
  let range = 0x264
  let is_stereo = true
end)

let play_music () =
  print_endline "loading music file";
  (* we can create binary music files with [ffmpeg]:
     ffmpeg -i minor_jumps_major_cats.wav -f u8 -ar 44.1k -ac 2 minor_jumps_major_cats.bin
     we can load it into memory with [ocaml-crunch]:
     ocaml-crunch music/ -e bin -o music.ml -m plain
  *)
  match Music.read "/papa_noel.bin" with
  | None -> failwith "no music"
  | Some bytes ->
      Seq.init (String.length bytes) Fun.id
      |> Lwt_seq.of_seq
      |> Lwt_seq.iter_s (fun i -> Pwm.write (Char.code bytes.[i]))

open Lwt.Syntax

let main_music () =
  let* r = Pwm.init () in
  Result.get_ok r;
  play_music ()

let cut = 16

let pattern =
  List.init 361 (fun i ->
      if i / cut mod 2 = 0 then
        if i mod 2 = 1 && abs ((cut / 2) - (i mod cut)) < 3 * cut / 12 then
          { Ws2812b.r = 45; b = 50; g = 0 }
        else { r = 0; g = 0; b = 0 }
      else
        let mode = i / cut in
        let pos = abs ((cut / 2) - (i mod cut)) in
        { r = 0; g = 30 - pos; b = 30 + (mode / 2 * pos) })

let next = function first :: next -> next @ [ first ] | _ -> assert false
let entropy = 15
let detropy = 2

let noise (target : Ws2812b.color) { Ws2812b.r; g; b } =
  let sample delta =
    if Random.bool () then
      if delta < 0 then max (-detropy) delta else min detropy delta
    else if delta < 0 then -entropy
    else entropy
  in
  let clip v = if v < 1 then 1 else if v > 255 then 255 else v in
  {
    Ws2812b.r = (if r = 0 then 0 else clip (sample (target.r - r) + r));
    g = (if g = 0 then 0 else clip (sample (target.g - g) + g));
    b = (if b = 0 then 0 else clip (sample (target.b - b) + b));
  }

let stop = ref false
let () = Sys.(set_signal sigint (Signal_handle (fun _ -> stop := true)))

open Lwt.Syntax

let main_led () =
  let c = 0 in
  let* _ = Pwm.init () in
  let* () = Lwt_unix.sleep 0.1 in
  let* v = Ws2812b.init () in

  let reset_frame =
    List.init 361 (fun _ -> { Ws2812b.r = 0; g = 0; b = 0 }) |> Ws2812b.encode v
  in

  let rec loop (pattern, target) c =
    let counter = Rpi.Mtime.counter () in
    let* () = Ws2812b.output (Ws2812b.encode v (pattern |> List.rev)) in
    let offset = Rpi.Mtime.counter_value_us counter in
    let* () = Rpi.Mtime.sleep_us (Int64.sub 16_000L offset) in
    (* 60 FPS *)
    if !stop then Ws2812b.output reset_frame
    else
      let next = if c mod 6 = 0 then next else Fun.id in
      let pattern = next pattern |> List.map2 noise target in
      loop (pattern, next target) (c + 1)
  in
  loop (pattern, pattern) c

let () = Lwt_main.run (Lwt.join [ main_led (); play_music () ])
