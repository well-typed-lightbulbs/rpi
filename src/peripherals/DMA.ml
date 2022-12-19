module Transfer_information = struct
  include Register.Raw

  let interrupt_enable = bool ~offset:0
  let two_d_mode = bool ~offset:1
  let wait_write_response = bool ~offset:3
  let destination_address_increment = bool ~offset:4

  (* 1 = 128-bit / 0 = 32-bit *)
  let destination_transfer_width = bool ~offset:5
  let destination_dreq = bool ~offset:6
  let destination_ignore = bool ~offset:7
  let source_address_increment = bool ~offset:8
  let source_transfer_width = bool ~offset:9
  let source_dreq = bool ~offset:10
  let source_ignore = bool ~offset:11
  let burst_length = int ~size:4 ~offset:12

  (* https://raspberrypi.stackexchange.com/questions/22881/what-values-in-the-dma-ti-permap-field-map-to-each-field *)
  type permap = No | Dsi | PcmTx | PcmRx | Smi | Pwm | SpiTx | SpiRx

  let peripheral_mapping =
    {
      offset = 16;
      size = 15;
      to_int =
        (function
        | No -> 0
        | Dsi -> 1
        | PcmTx -> 2
        | PcmRx -> 3
        | Smi -> 4
        | Pwm -> 5
        | SpiTx -> 6
        | SpiRx -> 7);
      of_int =
        (function
        | 0 -> No
        | 1 -> Dsi
        | 2 -> PcmTx
        | 3 -> PcmRx
        | 4 -> Smi
        | 5 -> Pwm
        | 6 -> SpiTx
        | 7 -> SpiRx
        | _ -> failwith "unk");
    }

  let waits = int ~size:5 ~offset:21
  let no_wide_bursts = bool ~offset:26

  type t = value
end

module Control_block = struct
  type t = {
    transfer_information : Transfer_information.t;
    source_address : Nativeint.t;
    destination_address : Nativeint.t;
    transfer_length : int;
    stride : int;
    next_control_address : int;
  }

  [%%cstruct
  type raw = {
    transfer_information : uint32_t;
    source_address : uint32_t;
    destination_address : uint32_t;
    transfer_length : uint32_t;
    stride : uint32_t;
    next_control_address : uint32_t;
    reserved0 : uint32_t;
    reserved1 : uint32_t;
  }
  [@@little_endian]]

  type lite = {
    transfer_information : Transfer_information.t;
    source_address : Nativeint.t;
    destination_address : Nativeint.t;
    transfer_length : int;
    next_control_address : int;
  }

  [%%cstruct
  type raw_lite = {
    transfer_information : uint32_t;
    source_address : uint32_t;
    destination_address : uint32_t;
    transfer_length : uint32_t;
    reserved0 : uint32_t;
    next_control_address : uint32_t;
    reserved1 : uint32_t;
    reserved2 : uint32_t;
  }
  [@@little_endian]]

  type v4 = {
    transfer_information : Transfer_information.t;
    source_address : Nativeint.t;
    source_information : int; (* TODO *)
    destination_address : Nativeint.t;
    destination_information : int; (* TODO *)
    transfer_length : int;
    next_control_address : int;
  }

  [%%cstruct
  type raw_4 = {
    transfer_information : uint32_t;
    source_address : uint32_t;
    source_information : uint32_t;
    destination_address : uint32_t;
    destination_information : uint32_t;
    transfer_length : uint32_t;
    next_control_address : uint32_t;
    reserved0 : uint32_t;
  }
  [@@little_endian]]

  let sizeof = sizeof_raw
  let sizeof_4 = sizeof_raw_4
  let sizeof_lite = sizeof_raw_lite

  let write buf (v : t) =
    set_raw_transfer_information buf
      (Int32.of_int (Transfer_information.reg v.transfer_information));
    set_raw_source_address buf (Nativeint.to_int32 v.source_address);
    set_raw_destination_address buf (Nativeint.to_int32 v.destination_address);
    set_raw_transfer_length buf (Int32.of_int v.transfer_length);
    set_raw_stride buf (Int32.of_int v.stride);
    set_raw_next_control_address buf (Int32.of_int v.next_control_address)

  let write_4 buf (v : v4) =
    set_raw_4_transfer_information buf
      (Int32.of_int (Transfer_information.reg v.transfer_information));
    set_raw_4_source_address buf (Nativeint.to_int32 v.source_address);
    set_raw_4_source_information buf (Int32.of_int v.source_information);
    set_raw_4_destination_address buf (Nativeint.to_int32 v.destination_address);
    set_raw_4_destination_information buf
      (Int32.of_int v.destination_information);
    set_raw_4_transfer_length buf (Int32.of_int v.transfer_length);
    set_raw_4_next_control_address buf (Int32.of_int v.next_control_address)

  let write_lite buf (v : lite) =
    set_raw_lite_transfer_information buf
      (Int32.of_int (Transfer_information.reg v.transfer_information));
    set_raw_lite_source_address buf (Nativeint.to_int32 v.source_address);
    set_raw_lite_destination_address buf
      (Nativeint.to_int32 v.destination_address);
    set_raw_lite_transfer_length buf (Int32.of_int v.transfer_length);
    set_raw_lite_next_control_address buf (Int32.of_int v.next_control_address)
end

type dma = DMA | DMA_Lite | DMA4

let dma_type = function
  | n when n >= 0 && n < 7 -> DMA
  | 7 | 8 | 9 | 10 -> DMA_Lite
  | 11 | 12 | 13 | 14 -> DMA4
  | _ -> invalid_arg "Invalid DMA channel"

module Make (M : sig
  val num : int
end) =
struct
  let base = Rpi_hardware.dma M.num

  module Reg = struct
    module Cs = struct
      (* Control and Status*)
      include Register.Make (struct
        let addr = base
      end)

      let reset = bool ~offset:31
      let abort = bool ~offset:30
      let disdebug = bool ~offset:29
      let wait_for_outstanding_writes = bool ~offset:28
      let error = bool ~offset:8
      let int = bool ~offset:2
      let end' = bool ~offset:1
      let active = bool ~offset:0
    end

    module Conblk_ad = struct
      (* Control Block Address *)
      include Register.Make (struct
        let addr = Mem.(base + 0x4n)
      end)

      let addr = int ~size:32 ~offset:0
    end

    module Debug = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x20n)
      end)
    end
  end
end
