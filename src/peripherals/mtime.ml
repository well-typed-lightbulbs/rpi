(* Time spans *)

type span_us = int64

(* Passing time *)

let timer_base = Mem.(Mmio.base + 0x00003000n)

let timer_clo = Mem.(timer_base + 0x04n)

let elapsed_us () = Mem.get_int64 timer_clo

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
