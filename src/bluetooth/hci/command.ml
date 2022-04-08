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

  type t =
    | Host_control of Host_control.t
    | LE_control of LE.t
    | Vendor of Vendor.t
    | OGF4 of OGF4.t
    | Unknown_ogc of (int * int)
    | Unknown_ogf of int

  let of_int v =
    let ogc = Int32.of_int (v land 0b0000001111111111) in
    let ogf = (v land 0b1111110000000000) lsr 10 in
    try
      match ogf with
      | v when v = Host_control.ogf ->
          Host_control (Host_control.int_to_t ogc |> Option.get)
      | v when v = LE.ogf -> LE_control (LE.int_to_t ogc |> Option.get)
      | v when v = Vendor.ogf -> Vendor (Vendor.int_to_t ogc |> Option.get)
      | v when v = OGF4.ogf -> OGF4 (OGF4.int_to_t ogc |> Option.get)
      | v -> Unknown_ogf v
    with Invalid_argument _ -> Unknown_ogc (ogf, Int32.to_int ogc)

  let v ~ogf ~ogc =
    let opcode = (ogf lsl 10) lor ogc in
    let b_hi = (opcode lsr 8) land 0xff in
    let b_lo = opcode land 0xff in
    (b_lo, b_hi)

  (*
  let make cmd =
    let ogf, ogc =
      match cmd with
      | Host_control hc -> (Host_control.ogf, Host_control.t_to_int hc)
      | LE_control hc -> (LE.ogf, LE.t_to_int hc)
      | Vendor hc -> (Vendor.ogf, Vendor.t_to_int hc)
      | OGF4 hc -> (OGF4.ogf, OGF4.t_to_int hc)
      | _ -> assert false
    in
    let opcode = (ogf lsl 10) lor Int32.to_int ogc in
    let b_hi = (opcode lsr 8) land 0xff in
    let b_lo = opcode land 0xff in
    (b_lo, b_hi)*)
end

type 'input t = {
  ogf : int;
  ogc : int;
  size : 'input -> int;
  write : 'input -> Cstruct.t -> unit;
}

type 'input command = 'input t

let v ogf ogc size write = { ogf; ogc; size; write }

module LE = struct
  let ogf = 8

  module Set_event_mask = struct
    type t = int

    let v =
      v ogf 1
        (fun _ -> 8)
        (fun mask buffer ->
          for i = 0 to 7 do
            Cstruct.set_uint8 buffer i mask
          done)
  end

  module Set_advertising_parameters = struct
    type t = {
      type_ : int;
      interval_min : int;
      interval_max : int;
      own_address_type : int;
      filter_policy : int;
    }

    let v =
      v ogf 6
        (fun _ -> 15)
        (fun {
               type_;
               interval_min;
               interval_max;
               own_address_type;
               filter_policy;
             } buffer ->
          Cstruct.LE.set_uint16 buffer 0 interval_min;
          Cstruct.LE.set_uint16 buffer 2 interval_max;
          Cstruct.set_uint8 buffer 4 type_;
          Cstruct.set_uint8 buffer 5 own_address_type;
          Cstruct.set_uint8 buffer 13 0x07;
          Cstruct.set_uint8 buffer 14 filter_policy)
  end

  module Set_advertising_data = struct
    type t = Advert_data.ad list

    let v =
      v ogf 8
        (fun _ -> 32)
        (fun lst buffer ->
          let total_size =
            List.fold_left
              (fun acc v -> acc + (2 + Advert_data.sizeof_data v))
              0 lst
          in
          if total_size > 31 then failwith "Advert data too big";
          Cstruct.set_uint8 buffer 0 total_size;
          List.fold_left
            (fun position item ->
              let sz = Advert_data.sizeof_data item in
              Advert_data.write_ad_to
                (Cstruct.sub buffer position (sz + 2))
                item;
              sz + 2 + position)
            1 lst
          |> ignore)
  end

  module Set_advertise_enable = struct
    type t = int

    let v =
      v ogf 10
        (fun _ -> 1)
        (fun enable buffer -> Cstruct.set_uint8 buffer 0 enable)
  end

  module Set_scan_parameters = struct
    type t = {
      type_ : int;
      interval : int;
      window : int;
      own_address_type : int;
      filter_policy : int;
    }

    let v =
      v ogf 11
        (fun _ -> 7)
        (fun { type_; interval; window; own_address_type; filter_policy } buffer ->
          Cstruct.set_uint8 buffer 0 type_;
          Cstruct.LE.set_uint16 buffer 1 interval;
          Cstruct.LE.set_uint16 buffer 3 window;
          Cstruct.set_uint8 buffer 5 own_address_type;
          Cstruct.set_uint8 buffer 6 filter_policy)
  end

  module Set_scan_enable = struct
    type t = { state : int; duplicates : int }

    let v =
      v ogf 12
        (fun _ -> 2)
        (fun { state; duplicates } buffer ->
          Cstruct.set_uint8 buffer 0 state;
          Cstruct.set_uint8 buffer 1 duplicates)
  end

  module Create_connection = struct
    type t = string

    let v =
      v ogf 13
        (fun _ -> 25)
        (fun addr v ->
          let ble_scan_interval = 60 in
          let ble_scan_window = 60 in
          let ble_scan_divisor = 0.625 in
          let p =
            Float.to_int (Float.of_int ble_scan_interval /. ble_scan_divisor)
          in
          let q =
            Float.to_int (Float.of_int ble_scan_window /. ble_scan_divisor)
          in
          let advert_min_freq = 100 in
          let advert_max_freq = 100 in
          let ble_granularity = 0.625 in
          let min_interval =
            Float.to_int (Float.of_int advert_min_freq /. ble_granularity)
          in
          let max_interval =
            Float.to_int (Float.of_int advert_max_freq /. ble_granularity)
          in
          Cstruct.LE.set_uint16 v 0 p;
          Cstruct.LE.set_uint16 v 2 q;
          Cstruct.set_char v 6 addr.[5];
          Cstruct.set_char v 7 addr.[4];
          Cstruct.set_char v 8 addr.[3];
          Cstruct.set_char v 9 addr.[2];
          Cstruct.set_char v 10 addr.[1];
          Cstruct.set_char v 11 addr.[0];
          Cstruct.LE.set_uint16 v 13 min_interval;
          Cstruct.LE.set_uint16 v 15 max_interval;
          Cstruct.set_uint8 v 19 0x2a;
          (* max_latency ? *)
          Cstruct.set_uint8 v 20 0x00)
  end
end

module Host_control = struct
  let ogf = 1

  module Reset = struct
    let v = v ogf 3 (fun _ -> 0) (fun () _ -> ())
  end

  module Set_event_mask = struct
    type t = {
      inquiry_complete : bool;
      (* 0 **)
      inquiry_result : bool;
      connection_complete : bool;
      connection_request : bool;
      disconnection_complete : bool;
      authentication_complete : bool;
      remote_name_request_complete : bool;
      (* 7 *)
      le_meta : bool; (* 61 *)
    }

    let bit n = function false -> 0 | true -> 1 lsl n

    let write t buffer =
      Cstruct.LE.set_uint64 buffer 0
        (Int64.of_int
           (bit 0 t.inquiry_complete lor bit 1 t.inquiry_result
           lor bit 2 t.connection_complete
           lor bit 3 t.connection_request
           lor bit 4 t.disconnection_complete
           lor bit 5 t.authentication_complete
           lor bit 6 t.remote_name_request_complete
           lor bit 61 t.le_meta))

    let v = v ogf 1 (fun _ -> 8) write
  end

  module Set_event_filter = struct
    type auto_accept = Off | Yes of { role_switch : bool }

    type connection_setup_filter =
      | Allow_all of auto_accept
      | Allow_class of {
          class_of_device : string;
          class_of_device_mask : string;
          auto_accept : auto_accept;
        }
      | Allow_bdaddr of { bdaddr : string; auto_accept : auto_accept }

    let sizeof_connction = function
      | Allow_all _ -> 2
      | Allow_class _ | Allow_bdaddr _ -> 8

    let write_connection_setup t buffer =
      match t with
      | Allow_all auto_accept -> (
          Cstruct.set_uint8 buffer 1 0x0;
          match auto_accept with
          | Off -> Cstruct.set_uint8 buffer 2 0x1
          | Yes { role_switch = false } -> Cstruct.set_uint8 buffer 2 0x2
          | Yes { role_switch = true } -> Cstruct.set_uint8 buffer 2 0x3)
      | Allow_class { class_of_device; class_of_device_mask; auto_accept } -> (
          Cstruct.set_uint8 buffer 1 0x1;
          Cstruct.set_char buffer 2 class_of_device.[2];
          Cstruct.set_char buffer 3 class_of_device.[1];
          Cstruct.set_char buffer 4 class_of_device.[0];
          Cstruct.set_char buffer 5 class_of_device_mask.[2];
          Cstruct.set_char buffer 6 class_of_device_mask.[1];
          Cstruct.set_char buffer 7 class_of_device_mask.[0];
          match auto_accept with
          | Off -> Cstruct.set_uint8 buffer 8 0x1
          | Yes { role_switch = false } -> Cstruct.set_uint8 buffer 8 0x2
          | Yes { role_switch = true } -> Cstruct.set_uint8 buffer 8 0x3)
      | Allow_bdaddr { bdaddr; auto_accept } -> (
          Cstruct.set_uint8 buffer 1 0x2;
          Cstruct.set_char buffer 2 bdaddr.[5];
          Cstruct.set_char buffer 3 bdaddr.[4];
          Cstruct.set_char buffer 4 bdaddr.[3];
          Cstruct.set_char buffer 5 bdaddr.[2];
          Cstruct.set_char buffer 6 bdaddr.[1];
          Cstruct.set_char buffer 7 bdaddr.[0];
          match auto_accept with
          | Off -> Cstruct.set_uint8 buffer 8 0x1
          | Yes { role_switch = false } -> Cstruct.set_uint8 buffer 8 0x2
          | Yes { role_switch = true } -> Cstruct.set_uint8 buffer 8 0x3)

    type inquiry_result_filter =
      | All_devices
      | Class of { class_of_device : string; class_of_device_mask : string }
      | Bdaddr of string

    let sizeof_inquiry = function All_devices -> 1 | Class _ | Bdaddr _ -> 7

    let write_inquiry_result t buffer =
      match t with
      | All_devices -> Cstruct.set_uint8 buffer 1 0x0
      | Class { class_of_device; class_of_device_mask } ->
          Cstruct.set_uint8 buffer 1 0x1;
          Cstruct.set_char buffer 2 class_of_device.[2];
          Cstruct.set_char buffer 3 class_of_device.[1];
          Cstruct.set_char buffer 4 class_of_device.[0];
          Cstruct.set_char buffer 5 class_of_device_mask.[2];
          Cstruct.set_char buffer 6 class_of_device_mask.[1];
          Cstruct.set_char buffer 7 class_of_device_mask.[0]
      | Bdaddr str ->
          Cstruct.set_uint8 buffer 1 0x2;
          Cstruct.set_char buffer 2 str.[5];
          Cstruct.set_char buffer 3 str.[4];
          Cstruct.set_char buffer 4 str.[3];
          Cstruct.set_char buffer 5 str.[2];
          Cstruct.set_char buffer 6 str.[1];
          Cstruct.set_char buffer 7 str.[0]

    type t =
      | Clear
      | Inquiry_result of inquiry_result_filter
      | Connection_setup of connection_setup_filter

    let sizeof = function
      | Clear -> 1
      | Inquiry_result v -> 1 + sizeof_inquiry v
      | Connection_setup v -> 1 + sizeof_connction v

    let write t buffer =
      match t with
      | Clear -> Cstruct.set_uint8 buffer 0 0x0
      | Inquiry_result v ->
          Cstruct.set_uint8 buffer 0 0x1;
          write_inquiry_result v buffer
      | Connection_setup v ->
          Cstruct.set_uint8 buffer 0 0x2;
          write_connection_setup v buffer

    let v = v ogf 5 sizeof write
  end
end

module OGF4 = struct
  let ogf = 0x04

  module Read_bdaddr = struct
    let v = v ogf 9 (fun _ -> 0) (fun () _ -> ())
  end
end

module Vendor = struct
  let ogf = 0x3f

  module Load_firmware = struct
    let v = v ogf 0x2e (fun _ -> 0) (fun () _ -> ())
  end

  module Set_baud = struct
    (* doc here: https://community.infineon.com/t5/Knowledge-Base-Articles/Using-Vendor-Specific-Commands-in-AnyCloud-Bluetooth/ta-p/267534*)
    let v =
      v ogf 0x18
        (fun _ -> 6)
        (fun baudrate buffer -> Cstruct.LE.set_uint32 buffer 2 baudrate)
  end

  module Set_bdaddr = struct
    let v =
      v ogf 0x01
        (fun _ -> 6)
        (fun bdaddr buffer ->
          Cstruct.set_char buffer 0 bdaddr.[5];
          Cstruct.set_char buffer 1 bdaddr.[4];
          Cstruct.set_char buffer 2 bdaddr.[3];
          Cstruct.set_char buffer 3 bdaddr.[2];
          Cstruct.set_char buffer 4 bdaddr.[1];
          Cstruct.set_char buffer 5 bdaddr.[0])
  end
end

let id = 0x01
let size (c : _ t) data = 3 + c.size data

let write (c : _ t) data buffer =
  let length = c.size data in
  let opcodebyte1, opcodebyte2 = Opcode.v ~ogf:c.ogf ~ogc:c.ogc in
  Cstruct.set_uint8 buffer 0 opcodebyte1;
  Cstruct.set_uint8 buffer 1 opcodebyte2;
  Cstruct.set_uint8 buffer 2 length;
  c.write data (Cstruct.sub buffer 3 length)
