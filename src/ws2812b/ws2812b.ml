type color = { r : int; (* 8 bits *)
                        g : int; (* 8 bits *)
                                 b : int (* 8 bits *) }

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
  let code state value =
    if value then (
      write state true;
      write state true;
      write state false)
    else (
      write state true;
      write state false;
      write state false)

  (* encode a color, msb first *)
  let color_code state value =
    for i = 7 downto 0 do
      code state (value land (1 lsl i) <> 0)
    done

  (* encode a light pattern *)
  let v pattern =
    let length = List.length pattern in
    let code_size = 3 * 24 * length in
    let number_of_words = (code_size + 31) / 32 in
    let state =
      { buffer = Array.make number_of_words 0; word_offset = 0; bit_offset = 0 }
    in
    List.iter
      (fun { r; g; b } ->
        color_code state g;
        color_code state r;
        color_code state b)
      pattern;
    state.buffer
end

type frame = int array

let encode = Encoding.v

open Rpi

let next_write_after = ref (Mtime.elapsed_us ())

(* 50 usec*)
let down_time = 50L

let wait_until_ready () =
  let current_time = Mtime.elapsed_us () in
  if current_time > !next_write_after then ()
  else Mtime.sleep_us (Int64.sub !next_write_after current_time)

let update_next_write_time () =
  next_write_after := Int64.add (Mtime.elapsed_us ()) down_time

let output data =
  wait_until_ready ();
  Pwm.init ();
  for i = 0 to Array.length data - 1 do
    Pwm.write data.(i)
  done;
  Pwm.stop ();
  update_next_write_time ()
