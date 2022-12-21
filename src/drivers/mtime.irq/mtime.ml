open Rpi.Mtime

(* Interrupts *)

type interrupt_line = L0 | L1 | L2 | L3

let interrupt_line_to_signal_number = function
  | L0 -> 0
  | L1 -> 1
  | L2 -> 2
  | L3 -> 3

let schedule_next_interrupt line target =
  let timer_compare =
    match line with
    | L0 -> Reg.timer_compare_0
    | L1 -> Reg.timer_compare_1
    | L2 -> Reg.timer_compare_2
    | L3 -> Reg.timer_compare_3
  in
  Mem.set_int timer_compare (Int64.to_int target)

let acknowledge_interrupt line =
  let cs =
    match line with L0 -> 0b0001 | L1 -> 0b0010 | L2 -> 0b0100 | L3 -> 0b1000
  in
  Mem.set_int Reg.timer_cs cs
