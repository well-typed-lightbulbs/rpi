include Peripheral.Make (struct
  let base = Mem.(Mmio.base + 0x00003000n)

  let registers_size = 0x1cn
end)

module type S = sig
  (** Monotonic time.

      [Mtime] gives access to the 64-bit free running system timer
      counter. Note that this time is independent from the CPU speed.

      {b References}
      {ul
      {- Section 12 of the {{:https://www.raspberrypi.org/documentation/hardware/raspberrypi/bcm2835/BCM2835-ARM-Peripherals.pdf}BCM2835 ARM Peripheral specification} (PDF).}} *)

  (** {1:spans Time spans} *)

  type span_us = int64
  (** The type for time spans in {e unsigned} microseconds. *)

  (** {1:passing Passing time} *)

  val elapsed_us : unit -> span_us
  (** [elapsed ()] is the number of microseconds elasped since boot
      time. *)

  val sleep_us : span_us -> unit
  (** [sleep_us d] blocks and sleeps for [d] microseconds. *)

  (** {1:counters Counters} *)

  type counter
  (** The type for counters. *)

  val counter : counter
  (** [counter ()] is a counter counting from call time on. *)

  val counter_value_us : counter -> span_us
  (** [counter_value_us c] is the current counter value in microseconds. *)

  (** {1:conv Time conversion} *)

  val s_to_us : int64
  (** [s_to_us] is the number of microseconds in one second. *)

  val ms_to_us : int64
  (** [ms_to_us] is the number of microseconds in one millisecond. *)
end

module Make (B : Base) = struct
  (* Time spans *)

  type span_us = int64

  (* Passing time *)

  let timer_clo = Mem.(B.base + 0x04n)

  let timer_chi = Mem.(B.base + 0x08n)

  let elapsed_us () =
    let low_32 = Mem.get_int timer_clo in
    let high_32 = Mem.get_int timer_chi in
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

  let counter = elapsed_us ()

  let counter_value_us c = Int64.sub (elapsed_us ()) c

  (* Time scale conversions *)

  let s_to_us = 1_000_000L

  let ms_to_us = 1_000L
end
