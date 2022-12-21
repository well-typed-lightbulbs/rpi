module type S = sig
  val read_byte : unit -> int Lwt.t
  val write_byte : int -> unit
end
(* interrupt-based reading *)

external irq_enable : unit -> unit = "irq_enable"

module UART0 = struct
  open Rpi
  open UART.UART0

  let stream, push = Lwt_stream.create ()
  let read_byte () = Lwt_stream.next stream
  let state = Ke.Rke.create ~capacity:1024 Bigarray.Int
  let pactl_cs = Mem.offset base 0x204e00

  let handler _ =
    Printf.printf "INTERRUPT %x %x %x\n%!" (Mem.get_int pactl_cs)
      (Reg.Ris.(read () && Register.raw) |> Optint.to_int)
      (Reg.Mis.(read () && Register.raw) |> Optint.to_int);
    Mem.dmb ();
    while not Reg.Fr.(read () && receive_fifo_empty) do
      let c = Reg.Dr.(read () && data) in
      Ke.Rke.push state c
    done;
    Reg.Icr.(empty |> set Register.raw (Optint.of_int 0x7ff) |> write);
    Mem.dmb ();
    irq_enable ()

  let rec restart_threads () =
    try
      let v = Ke.Rke.pop_exn state in
      push (Some v);
      restart_threads ()
    with Ke.Rke.Empty -> ()

  let uart_irq_line = 57

  let init () =
    (* UART SET UP*)
    Gpio.set_func P30 F_ALT3;
    Gpio.set_func P31 F_ALT3;
    Gpio.set_func P32 F_ALT3;
    Gpio.set_func P33 F_ALT3;
    Gpio.set_func P14 F_ALT5;
    Gpio.set_func P15 F_ALT5;
    flushrx ();

    (* mask interrupts *)
    Reg.Imsc.(empty |> set Register.raw (Optint.of_int 0x7ff) |> write);
    (* clear all interrupts *)
    Reg.Icr.(empty |> set Register.raw (Optint.of_int 0x7ff) |> write);
    Reg.Ibrd.(empty |> set integer_baud_rate_divisor 26 |> write);
    Reg.Fbrd.(empty |> set fractional_baud_rate_divisor 3 |> write);
    Reg.Ifls.(empty |> set receive_interrupt_fifo_level_select F1_8 |> write);
    Reg.Lcrh.(empty |> set enable_fifos true |> set word_length B8 |> write);
    Reg.Cr.(
      empty |> set uart_enable true |> set transmit_enable true
      |> set receiver_enable true |> set rts true |> write);
    (*
  Reg.Imsc.(
    empty
    |> set overrun_error_interrupt_mask true
    |> set transmit_interrupt_mask true
    |> set receive_interrupt_mask true
    |> write);*)
    Reg.Imsc.(empty |> set Register.raw (Optint.of_int 0x7ff) |> write);
    Reg.Icr.(empty |> set Register.raw (Optint.of_int 0x7ff) |> write);
    Mtime.sleep_us 10_000L;
    Sys.set_signal uart_irq_line (Sys.Signal_handle handler)

  let write_byte = write_byte
end