module type S = sig
  val read_byte : unit -> int

  val read_byte_ready : unit -> bool

  val write_byte : int -> unit
end

open Rpi

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
    end

    module Icr = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x44n)
      end)
    end

    module Ibrd = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x24n)
      end)
    end

    module Fbrd = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x28n)
      end)
    end

    module Ifls = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x34n)
      end)
    end

    module Lcrh = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x2cn)
      end)
    end

    module Cr = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x30n)
      end)
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
    Mem.set_int Reg.Imsc.addr 0x00;
    Mem.set_int Reg.Icr.addr 0x7ff;
    Mem.set_int Reg.Ibrd.addr 0x1a;
    Mem.set_int Reg.Fbrd.addr 0x03;
    Mem.set_int Reg.Ifls.addr 0x08;
    Mem.set_int Reg.Lcrh.addr 0x70;
    Mem.set_int Reg.Cr.addr 0xB01;
    Printf.printf "UART0 READY\n%!";
    Mem.set_int Reg.Imsc.addr 0x430;
    Mtime.sleep_us 10_000L
end
