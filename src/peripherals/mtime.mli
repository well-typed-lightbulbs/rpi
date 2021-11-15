include Peripheral.S

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

val elapsed_us : t -> span_us
(** [elapsed ()] is the number of microseconds elasped since boot
      time. *)

val sleep_us : t -> span_us -> unit
(** [sleep_us d] blocks and sleeps for [d] microseconds. *)

(** {1:counters Counters} *)

type counter
(** The type for counters. *)

val counter : t -> counter
(** [counter ()] is a counter counting from call time on. *)

val counter_value_us : t -> counter -> span_us
(** [counter_value_us c] is the current counter value in microseconds. *)

(** {1:conv Time conversion} *)

val s_to_us : int64
(** [s_to_us] is the number of microseconds in one second. *)

val ms_to_us : int64
(** [ms_to_us] is the number of microseconds in one millisecond. *)
