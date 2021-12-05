open Rpi

let reset_frame =
  List.init 361 (fun _ -> { Ws2812b.r = 0; g = 0; b = 0 }) |> Ws2812b.encode

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

let () =
  let c = 0 in
  let rec loop (pattern, target) c =
    let counter = Mtime.counter () in
    Ws2812b.output (Ws2812b.encode (pattern |> List.rev));
    let offset = Mtime.counter_value_us counter in
    Mtime.sleep_us (Int64.sub 32_000L offset);
    (* 60 FPS *)
    let pattern = next pattern |> List.map2 noise target in
    loop (pattern, next target) (c + 1)
  in
  loop (pattern, pattern) c
