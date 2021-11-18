include Peripheral.Make (struct
  let base `Rpi4 = Mem.(Mmio.base + 0x00003000n)

  let registers_size = 0x1cn
end)

(* Time spans *)

type span_us = int64

(* Passing time *)

let timer_clo base = Mem.(base + 0x04n)

let elapsed_us base = Mem.get_int64 (timer_clo base)

let sleep_us base d =
  (* That's a bit wasteful and unprecise because of allocs, FIXME
     wfi + timer IRQ *)
  let rec loop start =
    let e = Int64.sub (elapsed_us base) start in
    if Int64.compare e d < 0 then loop start else ()
  in
  loop (elapsed_us base)

(* Counters *)

type counter = span_us

let counter = elapsed_us

let counter_value_us base c = Int64.sub (elapsed_us base) c

(* Time scale conversions *)

let s_to_us = 1_000_000L

let ms_to_us = 1_000L
