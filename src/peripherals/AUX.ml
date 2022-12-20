let base = Rpi_hardware.aux

module Reg = struct
  module Irq = struct
    include Register.Make (struct
      let addr = base
    end)

    let miniuart = bool ~offset:0
    let spi1 = bool ~offset:1
    let spi2 = bool ~offset:2
  end

  module Mu_Io = struct
    include Register.Make (struct
      let addr = Mem.(offset base 0x40)
    end)

    let data = int ~offset:0 ~size:8
  end

  module Mu_Ier = struct
    include Register.Make (struct
      let addr = Mem.(offset base 0x44)
    end)

    let transmit_interrupt_enable = bool ~offset:0
    let receive_interrupt_enable = bool ~offset:1
  end

  module Mu_Iir = struct
    include Register.Make (struct
      let addr = Mem.(offset base 0x48)
    end)

    let rx_fifo_not_empty = bool ~offset:2
    let tx_fifo_empty = bool ~offset:1
    let interrupt_pending = bool ~offset:0
  end

  module Mu_Lsr = struct
    include Register.Make (struct
      let addr = Mem.(offset base 0x54)
    end)

    let receiver_overrun = bool ~offset:1
    let data_ready = bool ~offset:0
  end

  module Mu_Stat = struct
    include Register.Make (struct
      let addr = Mem.(offset base 0x64)
    end)

    let receiver_overrun = bool ~offset:4
    let symbol_available = bool ~offset:0
    let receive_fifo_fill_level = int ~offset:16 ~size:4
  end
end

external irq_enable : unit -> unit = "irq_enable"

let stream, push = Lwt_stream.create ()
let read () = Lwt_stream.next stream
let state = Ke.Rke.create ~capacity:1024 Bigarray.Char

let handler _ =
  Mem.dmb ();
  while Reg.Mu_Iir.(read rx_fifo_not_empty) do
    let c = Reg.Mu_Io.(read data) in
    Ke.Rke.push state (Char.unsafe_chr c)
  done;
  Mem.dmb ();
  irq_enable ()

let rec restart_threads () =
  try
    let v = Ke.Rke.pop_exn state in
    push (Some v);
    restart_threads ()
  with Ke.Rke.Empty -> ()

let aux_irq_line = 29

let init () =
  (* set up interrupts. global initialization is already done by gilbraltar. *)
  Sys.set_signal aux_irq_line (Sys.Signal_handle handler);
  Reg.Mu_Ier.(empty |> set transmit_interrupt_enable true |> write)
