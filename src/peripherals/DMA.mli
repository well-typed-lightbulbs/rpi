module Transfer_information : sig
  open Register
  include Raw_S

  val interrupt_enable : bool field
  val two_d_mode : bool field
  val wait_write_response : bool field
  val destination_address_increment : bool field
  val destination_transfer_width : bool field
  val destination_dreq : bool field
  val destination_ignore : bool field
  val source_address_increment : bool field
  val source_transfer_width : bool field
  val source_dreq : bool field
  val source_ignore : bool field
  val burst_length : int field

  type permap = No | Dsi | PcmTx | PcmRx | Smi | Pwm | SpiTx | SpiRx

  val peripheral_mapping : permap field
  val waits : int field
  val no_wide_bursts : bool field
end

module Control_block : sig
  type t = {
    transfer_information : Transfer_information.value;
    source_address : Optint.t;
    destination_address : Optint.t;
    transfer_length : int;
    stride : int;
    next_control_address : Optint.t;
  }

  val sizeof : int
  val set_raw_next_control_address : Cstruct.t -> Int32.t -> unit
  val set_raw_transfer_length : Cstruct.t -> Int32.t -> unit
  val write : Cstruct.t -> t -> unit

  type lite = {
    transfer_information : Transfer_information.value;
    source_address : Optint.t;
    destination_address : Optint.t;
    transfer_length : int;
    next_control_address : Optint.t;
  }

  val sizeof_lite : int
  val write_lite : Cstruct.t -> lite -> unit

  type v4 = {
    transfer_information : Transfer_information.value;
    source_address : Optint.t;
    source_information : int;
    destination_address : Optint.t;
    destination_information : int;
    transfer_length : int;
    next_control_address : Optint.t;
  }

  val sizeof_4 : int
  val write_4 : Cstruct.t -> v4 -> unit
end

type dma = DMA | DMA_Lite | DMA4

val dma_type : int -> dma

module Make : functor
  (M : sig
     val num : int
   end)
  -> sig
  val base : Optint.t

  module Reg : sig
    open Register

    module Cs : sig
      include S

      val reset : bool field
      val abort : bool field
      val disdebug : bool field
      val wait_for_outstanding_writes : bool field
      val error : bool field
      val int : bool field
      val end' : bool field
      val active : bool field
      val priority : int field
      val panic_priority : int field
    end

    module Conblk_ad : sig
      include S

      val addr : int field
    end

    module Source_ad : sig
      include S

      val addr : int field
    end

    module Txfr_len : sig
      include S

      val len : int field
    end

    module Debug : sig
      include S

      val addr : Optint.t
    end
  end
end
