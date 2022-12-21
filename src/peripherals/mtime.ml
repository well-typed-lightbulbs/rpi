let base = Rpi_hardware.mtime

(* Time spans *)

type span_us = int64

(* Passing time *)
module Reg = struct
  let timer_cs = base
  let timer_clo = Mem.offset base 0x04
  let timer_chi = Mem.offset base 0x08
  let timer_compare_0 = Mem.offset base 0x0c
  let timer_compare_1 = Mem.offset base 0x10
  let timer_compare_2 = Mem.offset base 0x14
  let timer_compare_3 = Mem.offset base 0x18
end

let elapsed_us () =
  let low_32 = Mem.get_int Reg.timer_clo in
  let high_32 = Mem.get_int Reg.timer_chi in
  let ( lsl ) = Int64.shift_left in
  let ( lor ) = Int64.logor in
  (Int64.of_int high_32 lsl 32) lor Int64.of_int low_32

let sleep_us d =
  (* That's a bit wasteful and unprecise because of allocs, FIXME
     wfi + timer IRQ *)
  let rec loop start =
    let e = Int64.sub (elapsed_us ()) start in
    if Int64.compare e d < 0 then loop start else ()
  in
  loop (elapsed_us ())

(* Counters *)

type counter = span_us

let counter = elapsed_us
let counter_value_us c = Int64.sub (elapsed_us ()) c

(* Time scale conversions *)

let s_to_us = 1_000_000L
let ms_to_us = 1_000L
