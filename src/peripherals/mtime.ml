let base = Rpi_hardware.mtime

(* Time spans *)

type span_us = int64

(* Passing time *)

let timer_cs = base

let timer_clo = Mem.(base + 0x04n)

let timer_chi = Mem.(base + 0x08n)

let timer_compare_0 = Mem.(base + 0x0cn)

let timer_compare_1 = Mem.(base + 0x10n)

let timer_compare_2 = Mem.(base + 0x14n)

let timer_compare_3 = Mem.(base + 0x18n)

let elapsed_us () =
  let low_32 = Mem.get_int timer_clo in
  let high_32 = Mem.get_int timer_chi in
  let ( lsl ) = Int64.shift_left in
  let ( lor ) = Int64.logor in
  (Int64.of_int high_32 lsl 32) lor Int64.of_int low_32

open Lwt.Syntax

let sleep_us_sync d =
  (* That's a bit wasteful and unprecise because of allocs, FIXME
     wfi + timer IRQ *)
  let rec loop start =
    let e = Int64.sub (elapsed_us ()) start in
    if Int64.compare e d < 0 then
      loop start
      else ()
  in
  loop (elapsed_us ())

let sleep_us d =
  (* That's a bit wasteful and unprecise because of allocs, FIXME
      wfi + timer IRQ *)
  let rec loop start =
    let e = Int64.sub (elapsed_us ()) start in
    if Int64.compare e d < 0 then
      let* () = Lwt.pause () in
        loop start
      else Lwt.return_unit
  in
  loop (elapsed_us ())

(* Counters *)

type counter = span_us

let counter = elapsed_us

let counter_value_us c = Int64.sub (elapsed_us ()) c

(* Time scale conversions *)

let s_to_us = 1_000_000L

let ms_to_us = 1_000L

let timer_1_set value = Mem.set_int timer_compare_1 value

(* Interrupts *)

type interrupt_line = L0 | L1 | L2 | L3

let interrupt_line_to_signal_number = function
  | L0 -> 0
  | L1 -> 1
  | L2 -> 2
  | L3 -> 3

let schedule_next_interrupt line target =
  let timer_compare = match line with
    | L0 -> timer_compare_0
    | L1 -> timer_compare_1
    | L2 -> timer_compare_2
    | L3 -> timer_compare_3
  in
  Mem.set_int timer_compare (Int64.to_int target)

let acknowledge_interrupt line =
  let cs = match line with
    | L0 -> 0b0001
    | L1 -> 0b0010
    | L2 -> 0b0100
    | L3 -> 0b1000
  in
  Mem.set_int timer_cs cs
