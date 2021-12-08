module Packet = struct
  let command = 0x01

  let acl = 0x02

  let event = 0x04
end

module Event = struct
  let disconnection_complete = 0x05

  let command_complete = 0x0e

  let command_status = 0x0f

  let le_meta_event = 0x3e

  let vendor_specific = 0xff
end

module Error = struct
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
  [@@uint32_t]]

  let of_int x = Int32.of_int x |> int_to_t |> Option.get
end

module Opcode = struct
  module LE = struct
    let ogf = 8

    [%%cenum
    type t =
      | NULL
      | Set_event_mask
      | Read_buffer_size
      | Read_local_supported_features
      | NULL2
      | Set_random_address
      | Set_advertising_parameters
      | Read_advertising_channel_tx_power
      | Set_advertising_data
      | Set_scan_response_data
      | Set_advertise_enable
      | Set_scan_parameters
      | Set_scan_enable
      | Create_connection
      | Create_connection_cancel
      | Read_white_list_size
      | Clear_white_lis
      | Add_device_to_white_list
      | Remove_device_from_white_list
      | Connection_update
      | Set_host_channel_classification
      | Read_channel_map
      | Read_remote_used_features
      | Encrypt
      | Rand
      | Start_encryption
      | Long_term_key_requested_reply
      | Long_term_key_requested_negative_reply
      | Read_supported_states
      | Receiver_test
      | Transmitter_test
      | Test_end_command
      | Remote_connection_parameter_request_reply
      | Remote_connection_parameter_request_negative_reply
      | Set_data_length
      | Read_suggested_default_data_length
      | Write_suggested_default_data_length
      | Read_local_p256_public_key
      | Generate_dhkey
      | Add_device_to_resolving_list
      | Remove_device_from_resolving_list
      | Clear_resolving_list
      | Read_resolving_list_size
      | Read_peer_resolvable_address
      | Read_local_resolvable_address
      | Set_address_resolution_enable
      | Set_resolvable_private_address_timeout
      | Read_maximum_data_length
    [@@uint32_t]]

    let () = assert (t_to_int Set_event_mask = 1l)

    let () = assert (t_to_int Read_maximum_data_length = 47l)
  end

  module OGF1 = struct
    let ogf = 1

    [%%cenum
    type t = Disconnect [@id 6] | Read_remote_version_information [@id 29]
    [@@uint32_t]]
  end

  module Host_control = struct
    let ogf = 1

    [%%cenum
    type t =
      | Set_event_mask [@id 1]
      | Reset [@id 3]
      | Set_event_filter [@id 5]
      | Read_transmit_power_level [@id 45]
      | Set_control_to_host_flow_control [@id 49]
      | Host_buffer_size [@id 51]
      | Host_number_of_completed_packets [@id 53]
      | Set_event_mask_page [@id 63]
    [@@uint32_t]]
  end

  module OGF4 = struct
    let ogf = 4

    [%%cenum
    type t = Read_local_version_information [@id 1] | Read_bdaddr [@id 9]
    [@@uint32_t]]
  end

  module Vendor = struct
    let ogf = 0x3f

    [%%cenum
    type t =
      | Set_bdaddr [@id 0x01]
      | Set_baud [@id 0x18]
      | Load_firmware [@id 0x2e]
    [@@uint32_t]]
  end

  type cmd =
    | Host_control of Host_control.t
    | LE_control of LE.t
    | Vendor of Vendor.t
    | OGF4 of OGF4.t

  let v cmd =
    let ogf, ogc =
      match cmd with
      | Host_control hc -> (Host_control.ogf, Host_control.t_to_int hc)
      | LE_control hc -> (LE.ogf, LE.t_to_int hc)
      | Vendor hc -> (Vendor.ogf, Vendor.t_to_int hc)
      | OGF4 hc -> (OGF4.ogf, OGF4.t_to_int hc)
    in
    let opcode = (ogf lsl 10) lor Int32.to_int ogc in
    let b_hi = (opcode lsr 8) land 0xff in
    let b_lo = opcode land 0xff in
    (b_lo, b_hi)
end

module LL = struct
  let scan_active = 0x01

  (* Connectable and scannable undirected advertising*)
  let adv_ind = 0x00

  (* Non connectable undirected advertising (ADV_NONCONN_IND *)
  let adv_nonconn_ind = 0x03
end
