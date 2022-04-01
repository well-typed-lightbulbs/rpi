module type S = sig
  val read_byte : unit -> int

  val read_byte_ready : unit -> bool

  val write_byte : int -> unit
end

module UART0 = struct
  let base = Rpi_hardware.uart0

  module Reg = struct
    module Dr = struct
      include Register.Make (struct
        let addr = base
      end)

      let data = int ~offset:0 ~size:8
    end

    module Fr = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x18n)
      end)

      let receive_fifo_empty = bool ~offset:4

      let transmit_fifo_full = bool ~offset:5
    end

    module Imsc = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x38n)
      end)

      let modem_interrupt_mask = bool ~offset:1
      let receive_interrupt_mask = bool ~offset:4
      let transmit_interrupt_mask = bool ~offset:5
      let receive_timeout_interrupt_mask = bool ~offset:6
      let framing_error_interrupt_mask = bool ~offset:7
      let parity_error_interrupt_mask = bool ~offset:8
      let break_error_interrupt_mask = bool ~offset:9
      let overrun_error_interrupt_mask = bool ~offset:10

    end

    module Icr = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x44n)
      end)

      let modem_interrupt_clear = bool ~offset:1
      let receive_interrupt_clear = bool ~offset:4
      let transmit_interrupt_clear = bool ~offset:5
      let receive_timeout_interrupt_clear = bool ~offset:6
      let framing_error_interrupt_clear = bool ~offset:7
      let parity_error_interrupt_clear = bool ~offset:8
      let break_error_interrupt_clear = bool ~offset:9
      let overrun_error_interrupt_clear = bool ~offset:10

    end

    module Ibrd = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x24n)
      end)

      let integer_baud_rate_divisor = int ~offset:0 ~size:16
    end

    module Fbrd = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x28n)
      end)

      let fractional_baud_rate_divisor = int ~offset:0 ~size:6
    end

    module Ifls = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x34n)
      end)

      type interrupt_fifo_level = F1_8 | F1_4 | F1_2 | F3_4 | F7_8

      let to_int = function
        | F1_8 -> 0 
        | F1_4 -> 1 
        | F1_2 -> 2 
        | F3_4 -> 3 
        | F7_8 -> 4

      let of_int = function
        | 0 -> F1_8 
        | 1 -> F1_4 
        | 2 -> F1_2 
        | 3 -> F3_4 
        | 4 -> F7_8
        | _ -> failwith "interrupt_fifo_level"

      let receive_interrupt_fifo_level_select = 
        {to_int; of_int; offset=3; size=3}
        
      let transmit_interrupt_fifo_level_select = 
        {to_int; of_int; offset=0; size=3}
    end

    module Lcrh = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x2cn)
      end)

      let stick_parity_select = bool ~offset:7

      type word_length = B8 | B7 | B6 | B5
      let word_length = {
        offset=5;
        size=2;
        to_int=
          (function
          | B8 -> 3
          | B7 -> 2
          | B6 -> 1
          | B5 -> 0);
        of_int=
          (function
          | 3 -> B8
          | 2 -> B7
          | 1 -> B6
          | 0 -> B5
          | _ -> failwith "word_length")
      }

      let enable_fifos = bool ~offset:4
      let two_stop_bits = bool ~offset:3
      let even_parity = bool ~offset:2
      let parity_enable = bool ~offset:1
      let break = bool ~offset:0
    end

    module Cr = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x30n)
      end)

      let cts_hardware_control_flow = bool ~offset:15
      let rts_hardware_control_flow = bool ~offset:14
      let rts = bool ~offset:11
      let receiver_enable = bool ~offset:9
      let transmit_enable = bool ~offset:8
      let loopback_enable = bool ~offset:7
      let uart_enable = bool~offset:0

    end
  end

  let read_byte_ready () = not Reg.Fr.(read receive_fifo_empty)

  let read_byte () =
    while Reg.Fr.(read receive_fifo_empty) do
      ()
    done;
    Reg.Dr.(read data)

  let flushrx () =
    while not Reg.Fr.(read receive_fifo_empty) do
      read_byte () |> ignore
    done

  let write_byte byte =
    while Reg.Fr.(read transmit_fifo_full) do
      ()
    done;
    Reg.Dr.(empty |> set data byte |> write)

  let init () =
    (* UART SET UP*)
    Gpio.set_func P30 F_ALT3;
    Gpio.set_func P31 F_ALT3;
    Gpio.set_func P32 F_ALT3;
    Gpio.set_func P33 F_ALT3;
    Gpio.set_func P14 F_ALT5;
    Gpio.set_func P15 F_ALT5;
    flushrx ();
    (* TODO: understand what's happening *)
    Reg.Imsc.(empty |> write); (* don't mask interrupts *)
    Mem.set_int Reg.Icr.addr 0x7ff; (* clear all interrupts *)
    Reg.Ibrd.(empty |> set integer_baud_rate_divisor 26 |> write);
    Reg.Fbrd.(empty |> set fractional_baud_rate_divisor 3 |> write);
    Reg.Ifls.(empty |> set receive_interrupt_fifo_level_select F1_4 |> write);
    Reg.Lcrh.(empty |> set enable_fifos true |> set word_length B8 |> write);
    Reg.Cr.(empty 
      |> set uart_enable true 
      |> set transmit_enable true 
      |> set receiver_enable true 
      |> set rts true
      |> write);
    Printf.printf "UART0 READY\n%!";
    Reg.Imsc.(empty 
      |> set overrun_error_interrupt_mask true
      |> set transmit_interrupt_mask true
      |> set receive_interrupt_mask true
      |> write);
    Mem.set_int Reg.Imsc.addr 0x430;
    Mtime.sleep_us 10_000L
end
 