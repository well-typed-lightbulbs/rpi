module UART0 = struct
  open Rpi
  open UART.UART0

  open Lwt.Syntax

  let read_byte () =
    Printf.printf "<..%!";
    (* wait for a byte to arrive *)
    let rec loop () =
      if Reg.Fr.(read () && receive_fifo_empty) then
        let* () = Lwt.pause () in loop ()
      else
        Lwt.return (
          (* read the byte *)
          Reg.Dr.(read () && data))
    in
    let+ v=  loop () in
    Printf.printf "\r<%02x\n%!" v;
    v

  let init () =
    (* UART SET UP*)
    Gpio.set_func P30 F_ALT3;
    Gpio.set_func P31 F_ALT3;
    Gpio.set_func P32 F_ALT3;
    Gpio.set_func P33 F_ALT3;
    Gpio.set_func P14 F_ALT0;
    Gpio.set_func P15 F_ALT0;
    flushrx ();

    (* mask interrupts *)
    Reg.Imsc.(empty |> set Register.raw (Optint.of_int 0x7ff) |> write);
    (* clear all interrupts *)
    Reg.Icr.(empty |> set Register.raw (Optint.of_int 0x7ff) |> write);
    Reg.Ibrd.(empty |> set integer_baud_rate_divisor 26 |> write);
    Reg.Fbrd.(empty |> set fractional_baud_rate_divisor 3 |> write);
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

    Printf.printf "INIT (%02x)\n%!" (Reg.Fr.(read () && Register.raw) |> Optint.to_int);
    while Reg.Fr.(read () && busy) do
      ()
    done


  let write_byte v =
    Printf.printf ">%02x (%02x)\n%!" v (Reg.Fr.(read () && Register.raw) |> Optint.to_int);
    write_byte v;
    Printf.printf ">>> (%02x)\n%!" (Reg.Fr.(read () && Register.raw) |> Optint.to_int);
end