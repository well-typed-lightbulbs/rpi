type 'data handler = { id : int; read : Cstruct.t -> 'data }

let v id read = { id; read }

module Status = struct
  [%%cenum
  type t =
    | OK
    | Unknown_command
    | Unknown_connection_identifier
    | Hardware_failure
    | Page_timeout
    | Authentication_failure
    | Pin_or_key_missing
    | Memory_capacity_exceeded
    | Connection_timeout
    | Connection_limit_exceeded
    | Synchronous_connection_limit_to_a_device_exceeded
    | Connection_already_exists
    | Command_disallowed
    | Connection_rejected_due_to_limited_ressources
    | Connection_rejected_due_to_security_reasons
    | Connection_rejected_due_to_unacceptable_bd_addr
    | Connection_accept_timeout_exceeded
    | Unsupported_feature_or_parameter_value
    | Invalid_hci_command_parameters
    | Remote_user_terminated_connection
    | Remote_device_terminated_connection_due_to_low_resources
    | Remote_device_terminated_connection_due_to_power_off
    | Connection_terminated_by_local_host
    | Repeated_attempts
    | Pairing_not_allowed
    | Unknown_lmp_pdu
    | Unsupported_remote_feature
    | Unspecified_error [@id 0x1f]
    | Packet_too_long [@id 0x45]
    | Unknown
  [@@uint32_t]]

  let of_int x = Int32.of_int x |> int_to_t |> Option.value ~default:Unknown
end

module Disconnection_complete = struct
  let id = 0x05

  type t = {
    status : Status.t;
    (* int16 *)
    handle : int;
    (* TODO *)
    reason : int;
  }

  let read buffer =
    let status = Cstruct.get_uint8 buffer 0 |> Status.of_int in
    let handle = Cstruct.LE.get_uint16 buffer 1 in
    let reason = Cstruct.get_uint8 buffer 3 in
    { status; handle; reason }

  let v = { id; read }
end

module Command_complete = struct
  type t = { command : Command.Opcode.t; data : Cstruct.t }

  let id = 0x0e

  let read buffer =
    let opcode = Cstruct.LE.get_uint16 buffer 1 in
    let command = Command.Opcode.of_int opcode in
    let data = Cstruct.sub buffer 3 (Cstruct.length buffer - 3) in
    { data; command }

  let v = { id; read }
end

module Command_status = struct
  type t = { command : Command.Opcode.t; status : Status.t }

  let id = 0x0f

  let read buffer =
    let status = Cstruct.get_uint8 buffer 0 |> Status.of_int in
    let opcode = Cstruct.LE.get_uint16 buffer 2 in
    let command = Command.Opcode.of_int opcode in
    { status; command }

  let v = { id; read }
end

module LE_meta_event = struct
  type t = { data : Cstruct.t }

  let id = 0x3e

  let read data = { data }

  let v = { id; read }
end

module Vendor_specific = struct
  type t = { data : Cstruct.t }

  let id = 0xff

  let read data = { data }

  let v = { id; read }
end

module Unknown = struct
  type t = { id : int; data : Cstruct.t }
end

let id = 0x04

type t =
  | Disconnection_complete of Disconnection_complete.t
  | Command_complete of Command_complete.t
  | Command_status of Command_status.t
  | LE_meta_event of LE_meta_event.t
  | Vendor_specific of Vendor_specific.t
  | Unknown of Unknown.t

let get_packet ~get_byte =
  let id = get_byte () in
  let size = get_byte () in
  let buffer = Cstruct.create_unsafe size in
  for i = 0 to size - 1 do
    Cstruct.set_uint8 buffer i (get_byte ())
  done;
  (id, buffer)

let read ~get_byte : t =
  let id, buffer = get_packet ~get_byte in
  match id with
  | v when v = Disconnection_complete.id ->
      Disconnection_complete (Disconnection_complete.read buffer)
  | v when v = Command_complete.id ->
      Command_complete (Command_complete.read buffer)
  | v when v = Command_status.id -> Command_status (Command_status.read buffer)
  | v when v = LE_meta_event.id -> LE_meta_event (LE_meta_event.read buffer)
  | v when v = Vendor_specific.id ->
      Vendor_specific (Vendor_specific.read buffer)
  | _ -> Unknown { Unknown.id; data = buffer }

exception Unexpected_event of int

let expect ~get_byte value =
  let id, buffer = get_packet ~get_byte in
  if id <> value.id then raise (Unexpected_event id) else value.read buffer
