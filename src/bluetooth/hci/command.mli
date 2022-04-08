module Opcode : sig
  module LE : sig
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
  end

  module OGF1 : sig
    type t = Disconnect | Read_remote_version_information
  end

  module Host_control : sig
    type t =
      | Set_event_mask
      | Reset
      | Set_event_filter
      | Read_transmit_power_level
      | Set_control_to_host_flow_control
      | Host_buffer_size
      | Host_number_of_completed_packets
      | Set_event_mask_page
  end

  module OGF4 : sig
    type t = Read_local_version_information | Read_bdaddr
  end

  module Vendor : sig
    val ogf : int

    type t = Set_bdaddr | Set_baud | Load_firmware
  end

  type t =
    | Host_control of Host_control.t
    | LE_control of LE.t
    | Vendor of Vendor.t
    | OGF4 of OGF4.t
    | Unknown_ogc of (int * int)
    | Unknown_ogf of int

  val of_int : int -> t
end

type 'input t
type 'input command = 'input t

val v : int -> int -> ('a -> int) -> ('a -> Cstruct.t -> unit) -> 'a t

module LE : sig
  module Set_event_mask : sig
    type t = int

    val v : int command
  end

  module Set_advertising_parameters : sig
    type t = {
      type_ : int;
      interval_min : int;
      interval_max : int;
      own_address_type : int;
      filter_policy : int;
    }

    val v : t command
  end

  module Set_advertising_data : sig
    type t = Advert_data.ad list

    val v : Advert_data.ad list command
  end

  module Set_advertise_enable : sig
    type t = int

    val v : int command
  end

  module Set_scan_parameters : sig
    type t = {
      type_ : int;
      interval : int;
      window : int;
      own_address_type : int;
      filter_policy : int;
    }

    val v : t command
  end

  module Set_scan_enable : sig
    type t = { state : int; duplicates : int }

    val v : t command
  end

  module Create_connection : sig
    type t = string

    val v : string command
  end
end

module Host_control : sig
  module Reset : sig
    val v : unit command
  end

  module Set_event_mask : sig
    type t = {
      inquiry_complete : bool;
      inquiry_result : bool;
      connection_complete : bool;
      connection_request : bool;
      disconnection_complete : bool;
      authentication_complete : bool;
      remote_name_request_complete : bool;
      le_meta : bool;
    }

    val bit : int -> bool -> int
    val write : t -> Cstruct.t -> unit
    val v : t command
  end

  module Set_event_filter : sig
    type auto_accept = Off | Yes of { role_switch : bool }

    type connection_setup_filter =
      | Allow_all of auto_accept
      | Allow_class of {
          class_of_device : string;
          class_of_device_mask : string;
          auto_accept : auto_accept;
        }
      | Allow_bdaddr of { bdaddr : string; auto_accept : auto_accept }

    type inquiry_result_filter =
      | All_devices
      | Class of { class_of_device : string; class_of_device_mask : string }
      | Bdaddr of string

    type t =
      | Clear
      | Inquiry_result of inquiry_result_filter
      | Connection_setup of connection_setup_filter

    val v : t command
  end
end

module OGF4 : sig
  val ogf : int

  module Read_bdaddr : sig
    val v : unit command
  end
end

module Vendor : sig
  val ogf : int

  module Load_firmware : sig
    val v : unit command
  end

  module Set_baud : sig
    val v : int32 command
  end

  module Set_bdaddr : sig
    val v : string command
  end
end

val id : int
val size : 'a t -> 'a -> int
val write : 'a t -> 'a -> Cstruct.t -> unit
