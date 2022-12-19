module Transfer_information : sig
  include module type of Register.Raw

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
    source_address : nativeint;
    destination_address : nativeint;
    transfer_length : int;
    stride : int;
    next_control_address : int;
  }

  val sizeof : int
  val write : Cstruct.t -> t -> unit

  type lite = {
    transfer_information : Transfer_information.value;
    source_address : nativeint;
    destination_address : nativeint;
    transfer_length : int;
    next_control_address : int;
  }

  val sizeof_lite : int
  val write_lite : Cstruct.t -> lite -> unit

  type v4 = {
    transfer_information : Transfer_information.value;
    source_address : nativeint;
    source_information : int;
    destination_address : nativeint;
    destination_information : int;
    transfer_length : int;
    next_control_address : int;
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
  val base : nativeint

  module Reg : sig
    module Cs : sig
      type 'a field = {
        offset : int;
        size : int;
        to_int : 'a -> int;
        of_int : int -> 'a;
      }

      val bool : offset:int -> bool field
      val get : int -> 'a field -> 'a

      type value

      val empty : value
      val set : 'a field -> 'a -> value -> value
      val reg : value -> int
      val read : 'a field -> 'a
      val write : value -> unit
      val addr : nativeint
      val reset : bool field
      val abort : bool field
      val disdebug : bool field
      val wait_for_outstanding_writes : bool field
      val error : bool field
      val int : bool field
      val end' : bool field
      val active : bool field
    end

    module Conblk_ad : sig
      type 'a field = {
        offset : int;
        size : int;
        to_int : 'a -> int;
        of_int : int -> 'a;
      }

      val int : size:int -> offset:int -> int field
      val bool : offset:int -> bool field
      val get : int -> 'a field -> 'a

      type value

      val empty : value
      val set : 'a field -> 'a -> value -> value
      val reg : value -> int
      val read : 'a field -> 'a
      val write : value -> unit
      val addr : int field
    end

    module Debug : sig
      type 'a field = {
        offset : int;
        size : int;
        to_int : 'a -> int;
        of_int : int -> 'a;
      }

      val int : size:int -> offset:int -> int field
      val bool : offset:int -> bool field
      val get : int -> 'a field -> 'a

      type value

      val empty : value
      val set : 'a field -> 'a -> value -> value
      val reg : value -> int
      val read : 'a field -> 'a
      val write : value -> unit
      val addr : nativeint
    end
  end
end
