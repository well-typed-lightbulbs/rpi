type color = {
  r : int; (* 8 bits *)
  g : int; (* 8 bits *)
  b : int; (* 8 bits *)
}

module Encoding = struct
  type state = {
    buffer : int array;
    mutable word_offset : int;
    mutable bit_offset : int;
  }

  (* write a bit to the buffer *)
  let write state value =
    if value then
      state.buffer.(state.word_offset) <-
        state.buffer.(state.word_offset) lor (1 lsl (31 - state.bit_offset));
    if state.bit_offset = 31 then (
      state.bit_offset <- 0;
      state.word_offset <- state.word_offset + 1)
    else state.bit_offset <- state.bit_offset + 1

  (* encode a bit as 1 -> 110, 0 -> 100 *)
  let code ~multiplier state value =
    for i = 0 to multiplier - 1 do
      match i * 3 / multiplier with
      | 0 -> write state true
      | 1 -> write state value
      | 2 -> write state false
      | _ -> failwith "non"
    done

  (* encode a color, msb first *)
  let color_code ~multiplier state value =
    for i = 7 downto 0 do
      code ~multiplier state (value land (1 lsl i) <> 0)
    done

  (* 60 usec *)
  let down_time_ns = 60 * 1000
  let frame_time_ns = 1250

  let pause_words ~multiplier =
    (multiplier * down_time_ns / frame_time_ns / 32) + 1

  let size ~multiplier length =
    let code_size = multiplier * 24 * length in
    ((code_size + 31) / 32) + pause_words ~multiplier

  (* encode a light pattern *)
  let v ~multiplier pattern =
    let length = List.length pattern in
    let number_of_words = size ~multiplier length in
    let state =
      { buffer = Array.make number_of_words 0; word_offset = 0; bit_offset = 0 }
    in
    List.iter
      (fun { r; g; b } ->
        color_code ~multiplier state g;
        color_code ~multiplier state r;
        color_code ~multiplier state b)
      pattern;
    state.buffer

  let write ~multiplier buf pattern =
    let array = v ~multiplier pattern in
    for i = 0 to Array.length array - 1 do
      Cstruct.LE.set_uint32 buf (4 * i) (Array.get array i |> Int32.of_int)
    done
end

type frame = int array

open Rpi

module Pwm = Pwm.Make (struct
  open Pwm

  let mode = Serial
  let pins = [ Pin18 ]
  let freq = 3 * 800_000
  let range = 32
  let is_stereo = false
end)

type t = { multiplier : int }

let init () =
  let v = Pwm.init () in
  let multiplier = match v with Ok () -> 3 | Error freq -> freq / 800_000 in
  assert (multiplier >= 3);
  Printf.printf "multiplier: %d\n%!" multiplier;
  { multiplier }

let encode { multiplier } = Encoding.v ~multiplier

let output data =
  for i = 0 to Array.length data - 1 do
    Pwm.write_sync data.(i)
  done

let encoded_size { multiplier } len = 4 * Encoding.size ~multiplier len

let write_cstruct { multiplier } buff data =
  Encoding.write ~multiplier buff data
