module Pwm = Rpi.Pwm.Make (struct
  let mode = Rpi.Pwm.Analog
  let pins = [Rpi.Pwm.Pin40; Rpi.Pwm.Pin41]
  let freq =  54 * 1000000 / 2
  let range = 0x264
  let is_stereo = true
end)

let play_music () =
  print_endline "loading music file";
  (* we can create binary music files with [ffmpeg]:
     ffmpeg -i audio.wav -f u8 -ar 44.1k -ac 2 audio.bin
     we can load it into memory with [ocaml-crunch]:
     ocaml-crunch music/ -e bin -o music.ml -m plain
     *)
  match Music.read "/audio.bin" with
  | None -> failwith "no music"
  | Some bytes -> String.iter (fun c -> Pwm.write (Char.code c)) bytes

let () =
  Pwm.init ();
  play_music ()
