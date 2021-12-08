open Rpi
module UART0 = UART.UART0

module type UART = UART.S

(* some docs:
   http://software-dl.ti.com/simplelink/esd/simplelink_cc13x2_sdk/1.60.00.29_new/exports/docs/ble5stack/vendor_specific_guide/BLE_Vendor_Specific_HCI_Guide/hci_interface.html
*)

(* cstruct ? *)
module BA = Bigarray.Array1

external bt_get_firmware :
  unit -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) BA.t
  = "caml_bt_get_firmware"

module Make (UART : UART.S) = struct
  let uart_assert_read value =
    let value' = UART.read_byte () in
    if value' != value then (
      Printf.printf "BT assertion failed: got %02x instead of %02x\n%!" value'
        value;
      assert false)

  let empty = Cstruct.empty

  type response =
    | Command_status
    | Command_complete of Cstruct.t
    | LE_meta_event of Cstruct.t
    | Vendor_specific of Cstruct.t
    | Unknown of (int * Cstruct.t)
    | Disconnection_complete of { status : int; handle : int; reason : int }

  let hci_parse_event (opcodebyte1, opcodebyte2) =
    match UART.read_byte () with
    | v when v = Hci.Event.command_status -> (
        uart_assert_read 4;
        (* PAYLOAD SIZE *)
        let code = UART.read_byte () in
        (* TODO: Num_HCI_Command_Packets *)
        assert (UART.read_byte () <> 0);
        (* TODO: responses can actually arrive out of order at some point *)
        uart_assert_read opcodebyte1;
        uart_assert_read opcodebyte2;
        match code with
        | 0 -> Ok Command_status
        | n -> Error (Hci.Error.of_int n))
    | v when v = Hci.Event.command_complete ->
        let payload_size = UART.read_byte () in
        (* Num_HCI_Command_Packets *)
        assert (UART.read_byte () <> 0);
        (* TODO: responses can actually arrive out of order at some point *)
        uart_assert_read opcodebyte1;
        uart_assert_read opcodebyte2;
        (* DATA *)
        let buffer = Cstruct.create_unsafe (payload_size - 3) in
        for i = 0 to payload_size - 3 - 1 do
          Cstruct.set_uint8 buffer i (UART.read_byte ())
        done;
        Ok (Command_complete buffer)
    | v when v = Hci.Event.le_meta_event ->
        let payload_size = UART.read_byte () in
        let buffer = Cstruct.create_unsafe payload_size in
        for i = 0 to payload_size - 1 do
          Cstruct.set_uint8 buffer i (UART.read_byte ())
        done;
        Ok (LE_meta_event buffer)
    | v when v = Hci.Event.disconnection_complete ->
        let payload_size = UART.read_byte () in
        let buffer = Cstruct.create_unsafe payload_size in
        for i = 0 to payload_size - 1 do
          Cstruct.set_uint8 buffer i (UART.read_byte ())
        done;
        let status = Cstruct.get_uint8 buffer 0 in
        let handle = Cstruct.LE.get_uint16 buffer 1 in
        let reason = Cstruct.get_uint8 buffer 3 in
        Ok (Disconnection_complete { status; reason; handle })
    | v when v = Hci.Event.vendor_specific ->
        let payload_size = UART.read_byte () in
        let buffer = Cstruct.create_unsafe payload_size in
        for i = 0 to payload_size - 1 do
          Cstruct.set_uint8 buffer i (UART.read_byte ())
        done;
        Ok (Vendor_specific buffer)
    | v ->
        let payload_size = UART.read_byte () in
        let buffer = Cstruct.create_unsafe payload_size in
        for i = 0 to payload_size - 1 do
          Cstruct.set_uint8 buffer i (UART.read_byte ())
        done;
        Printf.printf "Unknown event code %2x (%d)\n" v payload_size;
        Ok (Unknown (v, buffer))

  let hci_wait_event opcodes =
    uart_assert_read Hci.Packet.event;
    hci_parse_event opcodes

  let hci_parse_acl () =
    let handle =
      let handle_lo = UART.read_byte () in
      let handle_hi = UART.read_byte () in
      handle_lo lor (handle_hi lsl 8)
    in
    let length =
      let length_lo = UART.read_byte () in
      let length_hi = UART.read_byte () in
      length_lo lor (length_hi lsl 8)
    in
    let data = Cstruct.create_unsafe length in
    for i = 0 to length - 1 do
      Cstruct.set_uint8 data i (UART.read_byte ())
    done;
    Ok (handle, data)

  let hci_wait () =
    match UART.read_byte () with
    | v when v = Hci.Packet.event ->
        hci_parse_event (0, 0) |> Result.map (fun v -> `Event v)
    | v when v = Hci.Packet.acl ->
        hci_parse_acl () |> Result.map (fun v -> `Acl v)
    | _ -> assert false

  let hci_command_bytes (opcodebyte1, opcodebyte2) data =
    let length = Cstruct.length data in
    UART.write_byte Hci.Packet.command;
    UART.write_byte opcodebyte1;
    UART.write_byte opcodebyte2;
    UART.write_byte length;
    for i = 0 to length - 1 do
      let byte = Cstruct.get_uint8 data i in
      UART.write_byte byte
    done;
    hci_wait_event (opcodebyte1, opcodebyte2)

  let hci_command cmd = hci_command_bytes (Hci.Opcode.v cmd)

  let bt_reset () = hci_command Hci.Opcode.(Host_control Reset) empty

  let bt_load_firmware () =
    let data = Cstruct.of_bigarray (bt_get_firmware ()) in
    let size = Cstruct.length data in
    assert (size < 100000);
    hci_command Hci.Opcode.(Vendor Load_firmware) empty
    |> Result.get_ok |> ignore;

    let last_position = ref (-5.) in
    let maybe_print position =
      let percent = 100. *. Float.of_int position /. Float.of_int size in
      if percent -. !last_position >= 5. then (
        Printf.printf "%d/%d (%2.1f%%)\n%!" position size percent;
        last_position := percent)
    in

    let rec send position =
      if position >= size then ()
      else (
        maybe_print position;
        let opcode0 = Cstruct.get_byte data position in
        let opcode1 = Cstruct.get_byte data (position + 1) in
        let length = Cstruct.get_byte data (position + 2) in
        let slice = Cstruct.sub data (position + 3) length in
        hci_command_bytes (opcode0, opcode1) slice |> Result.get_ok |> ignore;
        send (position + length + 3))
    in
    send 0;
    Printf.printf "PATCH RAM: Success.\n%!";
    Mtime.sleep_us 250_000L

  let baudrate =
    (*  little endian, 115200 *)
    Cstruct.of_string "\x00\x00\x00\xc2\x01\x00"

  let bt_setbaud () = hci_command Hci.Opcode.(Vendor Set_baud) baudrate

  let bdaddr = Cstruct.of_string "\xee\xff\xc0\xee\xff\xc0"

  let bt_setbdaddr () = hci_command Hci.Opcode.(Vendor Set_bdaddr) bdaddr

  let bt_getbdaddr () =
    hci_command Hci.Opcode.(OGF4 Read_bdaddr) empty
    |> Result.map (function
         | Command_complete bdaddr -> Cstruct.to_bytes bdaddr
         | _ -> assert false)

  let lo n = n land 0xff

  let hi n = (n lsr 8) land 0xff

  let send_acl_subscribe handle =
    UART.write_byte Hci.Packet.acl;
    UART.write_byte (lo handle);
    UART.write_byte (hi handle);

    UART.write_byte 0x09;
    (* length *)
    UART.write_byte 0x00;

    UART.write_byte 0x05;
    (* data_length *)
    UART.write_byte 0x00;

    UART.write_byte 0x05;
    (* channel *)
    UART.write_byte 0x00;

    (* DATA *)
    [ 0x12; 0x2b; 0x00; 0x01; 0x00 ] |> List.iter UART.write_byte

  let set_event_mask mask =
    let command = Cstruct.create 8 in
    Cstruct.set_uint8 command 0 mask;
    Cstruct.set_uint8 command 1 mask;
    Cstruct.set_uint8 command 2 mask;
    Cstruct.set_uint8 command 3 mask;
    Cstruct.set_uint8 command 4 mask;
    Cstruct.set_uint8 command 5 mask;
    Cstruct.set_uint8 command 6 mask;
    Cstruct.set_uint8 command 7 mask;
    hci_command Hci.Opcode.(Host_control Set_event_mask) command

  let set_LE_event_mask mask =
    let command = Cstruct.create 8 in
    Cstruct.set_uint8 command 0 mask;
    Cstruct.set_uint8 command 1 mask;
    Cstruct.set_uint8 command 2 mask;
    Cstruct.set_uint8 command 3 mask;
    Cstruct.set_uint8 command 4 mask;
    Cstruct.set_uint8 command 5 mask;
    Cstruct.set_uint8 command 6 mask;
    Cstruct.set_uint8 command 7 mask;
    hci_command Hci.Opcode.(LE_control Set_event_mask) command

  let set_LE_scan_enable state duplicates =
    let command = Cstruct.create_unsafe 2 in
    Cstruct.set_uint8 command 0 state;
    Cstruct.set_uint8 command 1 duplicates;
    (* 0x0c*)
    hci_command Hci.Opcode.(LE_control Set_scan_enable) command

  let set_LE_scan_parameters type_ interval window own_address_type
      filter_policy =
    let command = Cstruct.create_unsafe 7 in
    Cstruct.set_uint8 command 0 type_;
    Cstruct.LE.set_uint16 command 1 interval;
    Cstruct.LE.set_uint16 command 3 window;
    Cstruct.set_uint8 command 5 own_address_type;
    Cstruct.set_uint8 command 6 filter_policy;

    hci_command Hci.Opcode.(LE_control Set_scan_parameters) command

  let set_LE_advert_enable state =
    let command = Cstruct.create_unsafe 1 in
    Cstruct.set_uint8 command 0 state;
    hci_command Hci.Opcode.(LE_control Set_advertise_enable) command

  let set_LE_advert_parameters type_ interval_min interval_max own_address_type
      filter_policy =
    let command = Cstruct.create 15 in
    Cstruct.LE.set_uint16 command 0 interval_min;
    Cstruct.LE.set_uint16 command 2 interval_max;
    Cstruct.set_uint8 command 4 type_;
    Cstruct.set_uint8 command 5 own_address_type;
    Cstruct.set_uint8 command 13 0x07;
    Cstruct.set_uint8 command 14 filter_policy;

    hci_command Hci.Opcode.(LE_control Set_advertising_parameters) command

  (* https://github.com/google/eddystone/blob/master/protocol-specification.md *)
  let eddystone_frame = Bytes.of_string "\x10\x00\x03isometim.es"

  let set_LE_advert_data () =
    let data =
      Advert_data.encode
        [
          Flags
            {
              le_limited_discoverable_mode = false;
              le_general_discoverable_mode = true;
              br_edr_not_supported = true;
              simultaneous_le_and_br_edr_to_the_same_device_capable_controller =
                false;
              simultaneous_le_and_br_edr_to_the_same_device_capable_host = false;
            };
          Complete_list_16bit_service_uuid [ 0xec00 ];
          Complete_local_name "echo service";
        ]
    in
    hci_command Hci.Opcode.(LE_control Set_advertising_data) data

  let stop_scanning () = set_LE_scan_enable 0 0

  let stop_advertising () = set_LE_advert_enable 0

  let start_active_scanning () =
    let ble_scan_interval = 60 in
    (* 60 ms*)
    let ble_scan_window = 60 in
    let ble_scan_divisor = 0.625 in
    let p = Float.to_int (Float.of_int ble_scan_interval /. ble_scan_divisor) in
    let q = Float.to_int (Float.of_int ble_scan_window /. ble_scan_divisor) in
    set_LE_scan_parameters Hci.LL.scan_active p q 0 0 |> ignore;
    set_LE_scan_enable 1 0

  let set_event_filter () =
    let command = Cstruct.create_unsafe 3 in
    Cstruct.set_uint8 command 0 0x02;
    Cstruct.set_uint8 command 1 0;
    Cstruct.set_uint8 command 2 0;

    hci_command Hci.Opcode.(Host_control Set_event_filter) command

  let start_active_advertising () =
    let advert_min_freq = 100 in
    let advert_max_freq = 100 in
    let ble_granularity = 0.625 in
    let min_interval =
      Float.to_int (Float.of_int advert_min_freq /. ble_granularity)
    in
    let max_interval =
      Float.to_int (Float.of_int advert_max_freq /. ble_granularity)
    in
    set_LE_advert_parameters Hci.LL.adv_ind min_interval max_interval 0 0
    |> Result.get_ok |> ignore;
    set_LE_advert_data () |> Result.get_ok |> ignore;
    set_LE_advert_enable 1 |> Result.get_ok |> ignore

  let connect addr =
    let ble_scan_interval = 60 in
    let ble_scan_window = 60 in
    let ble_scan_divisor = 0.625 in
    let p = Float.to_int (Float.of_int ble_scan_interval /. ble_scan_divisor) in
    let q = Float.to_int (Float.of_int ble_scan_window /. ble_scan_divisor) in
    let advert_min_freq = 100 in
    let advert_max_freq = 100 in
    let ble_granularity = 0.625 in
    let min_interval =
      Float.to_int (Float.of_int advert_min_freq /. ble_granularity)
    in
    let max_interval =
      Float.to_int (Float.of_int advert_max_freq /. ble_granularity)
    in
    let command =
      let open Cstruct in
      let v = create 25 in
      LE.set_uint16 v 0 p;
      LE.set_uint16 v 2 q;
      set_uint8 v 6 addr.(5);
      set_uint8 v 7 addr.(4);
      set_uint8 v 8 addr.(3);
      set_uint8 v 9 addr.(2);
      set_uint8 v 10 addr.(1);
      set_uint8 v 11 addr.(0);
      LE.set_uint16 v 13 min_interval;
      LE.set_uint16 v 15 max_interval;
      set_uint8 v 19 0x2a;
      (* max_latency ? *)
      set_uint8 v 20 0x00;
      v
    in
    hci_command Hci.Opcode.(LE_control Create_connection) command

  type hci_state = {
    mutable data_buf : bytes;
    mutable poll_state : int;
    mutable messages_received : int;
    mutable data_len : int;
  }

  let le_event_code = 0x3e

  let le_connect_code = 0x01

  let le_adreport_code = 0x02

  let hci_acl_pkt = 0x02

  let hci_event_pkt = 0x04

  let max_msg_len = 50

  let max_read_run = 100

  type connect_state = {
    mutable got_echo_sid : bool;
    mutable got_echo_name : bool;
    mutable echo_addr : string;
    mutable connected : bool;
    mutable connection_handle : int;
  }

  module LE = struct
    module Events = struct
      module Advertising_report = struct
        let id = 0x02

        type event_type =
          | Adv_ind
          | Adv_direct_ind
          | Adv_scan_ind
          | Adv_nonconn_ind
          | Scan_response

        let event_type_to_int = function
          | Adv_ind -> 0
          | Adv_direct_ind -> 1
          | Adv_scan_ind -> 2
          | Adv_nonconn_ind -> 3
          | Scan_response -> 4

        let event_type_of_int = function
          | 0 -> Adv_ind
          | 1 -> Adv_direct_ind
          | 2 -> Adv_scan_ind
          | 3 -> Adv_nonconn_ind
          | 4 -> Scan_response
          | _ -> assert false

        type address_type =
          | Public_device_address
          | Random_device_address
          | Public_identity_address
          | Random_identity_address

        let address_type_of_int = function
          | 0 -> Public_device_address
          | 1 -> Random_device_address
          | 2 -> Public_identity_address
          | 3 -> Random_identity_address
          | _ -> assert false

        type t = {
          event_type : event_type;
          address_type : address_type;
          address : string;
          data : Advert_data.ad list;
          rssi : int;
        }

        let parse buffer =
          let field size fn (offset, list) =
            List.fold_left_map
              (fun offset v ->
                let size = size v in
                let buffer = Cstruct.sub buffer offset size in
                (offset + size, (v, fn buffer)))
              offset list
          in

          let num_reports = Cstruct.get_byte buffer 0 in
          (1, List.init num_reports (fun _ -> ()))
          (* 1byte: event type *)
          |> field
               (fun _ -> 1)
               (fun buf -> Cstruct.get_byte buf 0 |> event_type_of_int)
          (* 1byte: address type *)
          |> field
               (fun _ -> 1)
               (fun buf -> Cstruct.get_byte buf 0 |> address_type_of_int)
          (* 6 bytes: address *)
          |> field (fun _ -> 6) Cstruct.to_string
          (* 1byte: data length *)
          |> field (fun _ -> 1) (fun buf -> Cstruct.get_byte buf 0)
          (* [data length] bytes: data *)
          |> field
               (fun (_, data_length) -> data_length)
               (fun buf -> Advert_data.decode buf)
          (* 1 byte: rssi *)
          |> fun (offset, list) ->
          List.fold_left_map
            (fun offset v -> (offset + 1, (v, Cstruct.get_byte buffer offset)))
            offset list
          |> snd
          |> List.map
               (fun
                 (((((((), event_type), address_type), address), _), data), rssi)
               -> { event_type; address_type; address; data; rssi })
      end

      module Connection_complete = struct
        let id = 0x01

        type t = {
          status : int;
          connection_handle : int;
          role : int;
          peer_address_type : int;
          peer_address : string;
          connection_interval : int;
          connection_latency : int;
          supervision_timeout : int;
          master_clock_accuracy : int;
        }

        let parse buf =
          let open Cstruct in
          {
            status = get_uint8 buf 0;
            connection_handle = LE.get_uint16 buf 1;
            role = get_uint8 buf 3;
            peer_address_type = get_uint8 buf 4;
            peer_address = Cstruct.to_string ~off:5 ~len:6 buf;
            connection_interval = LE.get_uint16 buf 11;
            connection_latency = LE.get_uint16 buf 13;
            supervision_timeout = LE.get_uint16 buf 15;
            master_clock_accuracy = get_uint8 buf 17;
          }
      end
    end
  end

  let bt_search () =
    let conn = ref None in
    while Option.is_none !conn do
      match hci_wait_event (0, 0) |> Result.get_ok with
      | LE_meta_event buffer
        when Cstruct.get_uint8 buffer 0 = LE.Events.Advertising_report.id ->
          Printf.printf "GOT DATA\n%!";
          let data =
            LE.Events.Advertising_report.parse
              (Cstruct.sub buffer 1 (Cstruct.length buffer - 1))
          in
          ()
      | _ -> assert false
    done;
    Option.get !conn

  let bt_wait_for_connection () =
    let conn = ref None in
    while Option.is_none !conn do
      match hci_wait_event (0, 0) |> Result.get_ok with
      | LE_meta_event buffer
        when Cstruct.get_uint8 buffer 0 = LE.Events.Connection_complete.id ->
          let data =
            LE.Events.Connection_complete.parse
              (Cstruct.sub buffer 1 (Cstruct.length buffer - 1))
          in
          Printf.printf "Connected to %S!\n%!" data.peer_address;
          conn := Some data
      | _ -> ()
    done;
    Option.get !conn

  let acl_respond_mtu handle mtu =
    let buffer = Cstruct.create_unsafe 11 in
    Cstruct.LE.set_uint16 buffer 0 handle;
    Cstruct.LE.set_uint16 buffer 2 7;
    Cstruct.LE.set_uint16 buffer 4 3;
    Cstruct.LE.set_uint16 buffer 6 4;
    (* mtu response opcode *)
    Cstruct.set_uint8 buffer 8 0x03;
    Cstruct.LE.set_uint16 buffer 9 mtu;

    UART.write_byte Hci.Packet.acl;
    for i = 0 to 10 do
      UART.write_byte (Cstruct.get_uint8 buffer i)
    done

  let acl_respond_attribues_request handle =
    let buffer = Cstruct.create_unsafe 16 in
    Cstruct.LE.set_uint16 buffer 0 handle;
    Cstruct.LE.set_uint16 buffer 2 12;
    (* ACL length *)
    Cstruct.LE.set_uint16 buffer 4 8;
    (* L2CAP length *)
    Cstruct.LE.set_uint16 buffer 6 4;
    (* L2CAP channel*)
    (* attribues response opcode *)
    Cstruct.set_uint8 buffer 8 0x11;
    Cstruct.set_uint8 buffer 9 0x06;
    (* another length*)
    Cstruct.LE.set_uint16 buffer 10 0x0001;
    (* attribute handle*)
    Cstruct.LE.set_uint16 buffer 12 0xffff;
    (* end group handle *)
    Cstruct.LE.set_uint16 buffer 14 0xec00;

    (* attribute value *)
    UART.write_byte Hci.Packet.acl;
    for i = 0 to 15 do
      UART.write_byte (Cstruct.get_uint8 buffer i)
    done

  let acl_read_by_type_request handle =
    let buffer = Cstruct.create_unsafe 17 in
    Cstruct.LE.set_uint16 buffer 0 handle;
    (* ACL length *)
    Cstruct.LE.set_uint16 buffer 2 13;
    (* L2CAP length *)
    Cstruct.LE.set_uint16 buffer 4 9;
    (* L2CAP channel*)
    Cstruct.LE.set_uint16 buffer 6 4;
    (* attribues response opcode *)
    Cstruct.set_uint8 buffer 8 0x09;
    (* another length*)
    Cstruct.set_uint8 buffer 9 0x07;
    (* attribute handle*)
    Cstruct.LE.set_uint16 buffer 10 0x0001;
    (* attribute value *)
    (* FLAGS: write without response *)
    Cstruct.set_uint8 buffer 12 0x8;
    Cstruct.LE.set_uint16 buffer 13 0x0001;
    Cstruct.LE.set_uint16 buffer 15 0xec0e;

    UART.write_byte Hci.Packet.acl;
    for i = 0 to 16 do
      UART.write_byte (Cstruct.get_uint8 buffer i)
    done

  let acl_read_by_type_request_client_char_config handle =
    let buffer = Cstruct.create_unsafe 14 in
    Cstruct.LE.set_uint16 buffer 0 handle;
    (* ACL length *)
    Cstruct.LE.set_uint16 buffer 2 10;
    (* L2CAP length *)
    Cstruct.LE.set_uint16 buffer 4 6;
    (* L2CAP channel*)
    Cstruct.LE.set_uint16 buffer 6 4;
    (* attribues response opcode *)
    Cstruct.set_uint8 buffer 8 0x09;
    (* another length*)
    Cstruct.set_uint8 buffer 9 0x04;
    (* attribute handle*)
    Cstruct.LE.set_uint16 buffer 10 0x0001;
    (* attribute value *)
    Cstruct.LE.set_uint16 buffer 12 0x00;

    UART.write_byte Hci.Packet.acl;
    for i = 0 to 13 do
      UART.write_byte (Cstruct.get_uint8 buffer i)
    done

  let acl_error handle =
    let buffer = Cstruct.create_unsafe 13 in
    Cstruct.LE.set_uint16 buffer 0 handle;
    (* ACL length *)
    Cstruct.LE.set_uint16 buffer 2 9;
    (* L2CAP length *)
    Cstruct.LE.set_uint16 buffer 4 5;
    (* L2CAP channel*)
    Cstruct.LE.set_uint16 buffer 6 4;
    (* attribues response opcode *)
    Cstruct.set_uint8 buffer 8 0x01;
    (* request opcode in error *)
    Cstruct.set_uint8 buffer 9 0x08;
    (* attribute handle in error *)
    Cstruct.LE.set_uint16 buffer 10 0x0001;
    (* attribute_not_found error code *)
    Cstruct.set_uint8 buffer 12 0x0A;

    UART.write_byte Hci.Packet.acl;
    for i = 0 to 12 do
      UART.write_byte (Cstruct.get_uint8 buffer i)
    done

  let ack handle =
    let buffer = Cstruct.create 9 in
    Cstruct.LE.set_uint16 buffer 0 handle;
    (* ACL length *)
    Cstruct.LE.set_uint16 buffer 2 5;
    (* L2CAP length *)
    Cstruct.LE.set_uint16 buffer 4 1;
    (* L2CAP channel*)
    Cstruct.LE.set_uint16 buffer 6 4;
    (* attribues response opcode *)
    Cstruct.set_uint8 buffer 8 0x13;

    UART.write_byte Hci.Packet.acl;
    for i = 0 to 8 do
      UART.write_byte (Cstruct.get_uint8 buffer i)
    done

  let bt_wait_for_data () =
    let conn = ref None in
    while Option.is_none !conn do
      match hci_wait () |> Result.get_ok with
      | `Event (LE_meta_event buffer) ->
          Printf.printf "Unknown event %02x\n%!" (Cstruct.get_uint8 buffer 0)
      | `Event (Unknown (i, _)) -> Printf.printf "Ignored event %2x.\n%!" i
      | `Event _ -> Printf.printf "Ignored known event.\n%!"
      | `Acl (handle, buffer) ->
          let _length = Cstruct.LE.get_uint16 buffer 0 in
          let channel = Cstruct.LE.get_uint16 buffer 2 in
          if channel == 4 then
            let opcode = Cstruct.get_uint8 buffer 4 in
            if opcode == 2 then (
              let mtu = Cstruct.LE.get_uint16 buffer 5 in
              Printf.printf "MTU request: %d\n%!" mtu;
              acl_respond_mtu handle mtu)
            else if opcode == 0x10 then (
              let uuid = Cstruct.LE.get_uint16 buffer 9 in
              Printf.printf "attribues request: %2x\n%!" uuid;
              acl_respond_attribues_request handle)
            else if opcode == 0x08 then (
              let start = Cstruct.LE.get_uint16 buffer 5 in
              let uuid = Cstruct.LE.get_uint16 buffer 9 in
              Printf.printf "attribues type: %2x / %2x\n%!" start uuid;
              if start == 1 then
                if uuid == 0x2803 then acl_read_by_type_request handle
                else acl_read_by_type_request_client_char_config handle
              else acl_error handle)
            else if opcode == 0x12 then (
              let uuid = Cstruct.LE.get_uint16 buffer 5 in
              let data = Cstruct.to_string ~off:7 buffer in
              Printf.printf "Write %s to %02x\n%!" data uuid;
              ack handle)
    done;
    Option.get !conn
end
