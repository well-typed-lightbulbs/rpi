

module Transfer_information = struct
  open Register.Raw

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

  let peripheral_mapping = int ~size:5 ~offset:16

  let waits = int ~size:5 ~offset:21

  let no_wide_bursts = bool ~offset:26
end


[%%cstruct
type control_block = {
  transfer_information: uint32_t; 
  source_address: uint32_t;
  destination_address: uint32_t;
  transfer_length: uint32_t;
  stride: uint32_t;
  next_control_address: uint32_t;
  reserved0: uint32_t;
  reserved1: uint32_t;
} [@@little_endian]]


