module UART0 = struct
  let base = Rpi_hardware.uart0

  module Reg = struct
    open Register

    module Dr = struct
      include Make (struct
        let addr = base
      end)

      let data = int ~offset:0 ~size:8
    end

    module Fr = struct
      include Make (struct
        let addr = Mem.offset base 0x18
      end)

      let busy = bool ~offset:3
      let receive_fifo_empty = bool ~offset:4
      let transmit_fifo_full = bool ~offset:5
    end

    module Imsc = struct
      include Make (struct
        let addr = Mem.offset base 0x38
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
      include Make (struct
        let addr = Mem.offset base 0x44
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
      include Make (struct
        let addr = Mem.offset base 0x24
      end)

      let integer_baud_rate_divisor = int ~offset:0 ~size:16
    end

    module Fbrd = struct
      include Make (struct
        let addr = Mem.offset base 0x28
      end)

      let fractional_baud_rate_divisor = int ~offset:0 ~size:6
    end

    module Ifls = struct
      include Make (struct
        let addr = Mem.offset base 0x34
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
        { to_int; of_int; offset = 3; size = 3 }

      let transmit_interrupt_fifo_level_select =
        { to_int; of_int; offset = 0; size = 3 }
    end

    module Lcrh = struct
      include Make (struct
        let addr = Mem.offset base 0x2c
      end)

      let stick_parity_select = bool ~offset:7

      type word_length = B8 | B7 | B6 | B5

      let word_length =
        {
          offset = 5;
          size = 2;
          to_int = (function B8 -> 3 | B7 -> 2 | B6 -> 1 | B5 -> 0);
          of_int =
            (function
            | 3 -> B8
            | 2 -> B7
            | 1 -> B6
            | 0 -> B5
            | _ -> failwith "word_length");
        }

      let enable_fifos = bool ~offset:4
      let two_stop_bits = bool ~offset:3
      let even_parity = bool ~offset:2
      let parity_enable = bool ~offset:1
      let break = bool ~offset:0
    end

    module Cr = struct
      include Make (struct
        let addr = Mem.offset base 0x30
      end)

      let cts_hardware_control_flow = bool ~offset:15
      let rts_hardware_control_flow = bool ~offset:14
      let rts = bool ~offset:11
      let receiver_enable = bool ~offset:9
      let transmit_enable = bool ~offset:8
      let loopback_enable = bool ~offset:7
      let uart_enable = bool ~offset:0
    end

    module Ris = struct
      include Make (struct
        let addr = Mem.offset base 0x3c
      end)
    end

    module Mis = struct
      include Make (struct
        let addr = Mem.offset base 0x40
      end)
    end
  end

  let read_byte_ready () = not Reg.Fr.(read () && receive_fifo_empty)

  let read_byte_sync () =
    while Reg.Fr.(read () && receive_fifo_empty) do
      ()
    done;
    Reg.Dr.(read () && data)

  let flushrx () =
    while not Reg.Fr.(read () && receive_fifo_empty) do
      read_byte_sync () |> ignore
    done

  let write_byte byte =
    while Reg.Fr.(read () && transmit_fifo_full) do
      ()
    done;
    Reg.Dr.(empty |> set data byte |> write)

  let read () =
    (* wait for a byte to arrive *)
    while Reg.Fr.(read () && receive_fifo_empty) do
      Mtime.sleep_us 10L
    done;
    (* read the byte *)
    Reg.Dr.(read () && data)
end
