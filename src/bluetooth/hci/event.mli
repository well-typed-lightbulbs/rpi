type 'data handler 

module Status : sig
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
    | Unspecified_error
    | Packet_too_long
    | Unknown
end

module Disconnection_complete : sig
  type t = { status : Status.t; handle : int; reason : int }

  val v : t handler
end

module Command_complete : sig
  type t = { command : Command.Opcode.t; data : Cstruct.t }

  val v : t handler
end

module Command_status : sig
  type t = { command : Command.Opcode.t; status : Status.t }

  val v : t handler
end

module LE_meta_event : sig
  type t = { data : Cstruct.t }

  val v : t handler
end

module Vendor_specific : sig
  type t = { data : Cstruct.t }

  val v : t handler
end

module Unknown : sig
  type t = { id : int; data : Cstruct.t }
end

val id : int

type t =
  | Disconnection_complete of Disconnection_complete.t
  | Command_complete of Command_complete.t
  | Command_status of Command_status.t
  | LE_meta_event of LE_meta_event.t
  | Vendor_specific of Vendor_specific.t
  | Unknown of Unknown.t

val read : get_byte:(unit -> int Lwt.t) -> t Lwt.t

exception Unexpected_event of int

val expect : get_byte:(unit -> int Lwt.t) -> 'a handler -> 'a Lwt.t
