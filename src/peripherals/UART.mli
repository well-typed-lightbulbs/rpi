module UART0 : sig
  val base : Optint.t

  module Reg : sig
    open Register

    module Dr : sig
      include S

      val data : int field
    end

    module Fr : sig
      include S

      val busy : bool field
      val receive_fifo_empty : bool field
      val transmit_fifo_full : bool field
    end

    module Imsc : sig
      include S

      val modem_interrupt_mask : bool field
      val receive_interrupt_mask : bool field
      val transmit_interrupt_mask : bool field
      val receive_timeout_interrupt_mask : bool field
      val framing_error_interrupt_mask : bool field
      val parity_error_interrupt_mask : bool field
      val break_error_interrupt_mask : bool field
      val overrun_error_interrupt_mask : bool field
    end

    module Icr : sig
      include S

      val modem_interrupt_clear : bool field
      val receive_interrupt_clear : bool field
      val transmit_interrupt_clear : bool field
      val receive_timeout_interrupt_clear : bool field
      val framing_error_interrupt_clear : bool field
      val parity_error_interrupt_clear : bool field
      val break_error_interrupt_clear : bool field
      val overrun_error_interrupt_clear : bool field
    end

    module Ibrd : sig
      include S

      val integer_baud_rate_divisor : int field
    end

    module Fbrd : sig
      include S

      val fractional_baud_rate_divisor : int field
    end

    module Ifls : sig
      include S

      type interrupt_fifo_level = F1_8 | F1_4 | F1_2 | F3_4 | F7_8

      val to_int : interrupt_fifo_level -> int
      val of_int : int -> interrupt_fifo_level
      val receive_interrupt_fifo_level_select : interrupt_fifo_level field
      val transmit_interrupt_fifo_level_select : interrupt_fifo_level field
    end

    module Lcrh : sig
      include S

      val stick_parity_select : bool field

      type word_length = B8 | B7 | B6 | B5

      val word_length : word_length field
      val enable_fifos : bool field
      val two_stop_bits : bool field
      val even_parity : bool field
      val parity_enable : bool field
      val break : bool field
    end

    module Cr : sig
      include S

      val cts_hardware_control_flow : bool field
      val rts_hardware_control_flow : bool field
      val rts : bool field
      val receiver_enable : bool field
      val transmit_enable : bool field
      val loopback_enable : bool field
      val uart_enable : bool field
    end

    module Ris : sig
      include S
    end

    module Mis : sig
      include S
    end
  end

  val read_byte_ready : unit -> bool
  val read_byte_sync : unit -> int
  val flushrx : unit -> unit
  val write_byte : int -> unit
  val read : unit -> int
end
