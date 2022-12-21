let base = Rpi_hardware.aux

module Reg = struct
  open Register

  module Irq = struct
    include Make (struct
      let addr = base
    end)

    let miniuart = bool ~offset:0
    let spi1 = bool ~offset:1
    let spi2 = bool ~offset:2
  end

  module Mu_Io = struct
    include Make (struct
      let addr = Mem.(offset base 0x40)
    end)

    let data = int ~offset:0 ~size:8
  end

  module Mu_Ier = struct
    include Make (struct
      let addr = Mem.(offset base 0x44)
    end)

    let transmit_interrupt_enable = bool ~offset:0
    let receive_interrupt_enable = bool ~offset:1
  end

  module Mu_Iir = struct
    include Make (struct
      let addr = Mem.(offset base 0x48)
    end)

    let rx_fifo_not_empty = bool ~offset:2
    let tx_fifo_empty = bool ~offset:1
    let interrupt_pending = bool ~offset:0
  end

  module Mu_Lsr = struct
    include Make (struct
      let addr = Mem.(offset base 0x54)
    end)

    let receiver_overrun = bool ~offset:1
    let data_ready = bool ~offset:0
  end

  module Mu_Stat = struct
    include Make (struct
      let addr = Mem.(offset base 0x64)
    end)

    let receiver_overrun = bool ~offset:4
    let symbol_available = bool ~offset:0
    let receive_fifo_fill_level = int ~offset:16 ~size:4
  end
end
