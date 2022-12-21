module Reg : sig
  open Register

  module Irq : sig
    include S

    val miniuart : bool field
    val spi1 : bool field
    val spi2 : bool field
  end

  module Mu_Io : sig
    include S

    val data : int field
  end

  module Mu_Ier : sig
    include S

    val transmit_interrupt_enable : bool field
    val receive_interrupt_enable : bool field
  end

  module Mu_Iir : sig
    include S

    val rx_fifo_not_empty : bool field
    val tx_fifo_empty : bool field
    val interrupt_pending : bool field
  end

  module Mu_Lsr : sig
    include S

    val receiver_overrun : bool field
    val data_ready : bool field
  end

  module Mu_Stat : sig
    include S

    val receiver_overrun : bool field
    val symbol_available : bool field
    val receive_fifo_fill_level : int field
  end
end
