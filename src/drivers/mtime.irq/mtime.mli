(** Interruptions *)

type interrupt_line = L0 | L1 | L2 | L3

val interrupt_line_to_signal_number : interrupt_line -> int
val schedule_next_interrupt : interrupt_line -> Rpi.Mtime.span_us -> unit
val acknowledge_interrupt : interrupt_line -> unit
