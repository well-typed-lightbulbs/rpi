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

  let write x = Hci.write ~write_byte:UART.write_byte x

  let expect x = Hci.expect ~get_byte:UART.read_byte x

  let bt_reset () =
    write (Command Hci.Packet.Command.Host_control.Reset.v) ();
    expect (Event Hci.Packet.Event.Command_complete.v)

  let bt_load_firmware () =
    let data = Cstruct.of_bigarray (bt_get_firmware ()) in
    let size = Cstruct.length data in
    assert (size < 100000);
    write (Command Hci.Packet.Command.Vendor.Load_firmware.v) ();
    expect (Event Hci.Packet.Event.Command_complete.v) |> ignore;

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

        UART.write_byte Hci.Packet.Command.id;
        UART.write_byte opcode0;
        UART.write_byte opcode1;
        UART.write_byte length;
        for i = 0 to length - 1 do
          let byte = Cstruct.get_uint8 slice i in
          UART.write_byte byte
        done;
        expect (Event Hci.Packet.Event.Command_complete.v) |> ignore;

        send (position + length + 3))
    in
    send 0;
    Printf.printf "PATCH RAM: Success.\n%!";
    Mtime.sleep_us 250_000L

  let bt_setbaud () =
    write (Command Hci.Packet.Command.Vendor.Set_baud.v) 115200l;
    expect (Event Hci.Packet.Event.Command_complete.v)

  let bt_setbdaddr () =
    write (Command Hci.Packet.Command.Vendor.Set_bdaddr.v)
      "\xc0\xff\xee\xc0\xff\xee";
    expect (Event Hci.Packet.Event.Command_complete.v)

  let bt_getbdaddr () =
    write (Command Hci.Packet.Command.OGF4.Read_bdaddr.v) ();
    let { Hci.Packet.Event.Command_complete.data; _ } =
      expect (Event Hci.Packet.Event.Command_complete.v)
    in
    (* answer is a byte and then the little-endian bdaddr *)
    let buffer = Cstruct.create_unsafe 6 in
    for i = 0 to 5 do
      Cstruct.set_uint8 buffer i (Cstruct.get_uint8 data (1 + (5 - i)))
    done;
    Cstruct.to_string buffer

  let set_event_mask mask =
    write (Command Hci.Packet.Command.Host_control.Set_event_mask.v) mask;
    expect (Event Hci.Packet.Event.Command_complete.v)

  let set_LE_event_mask mask =
    write (Command Hci.Packet.Command.LE.Set_event_mask.v) mask;
    expect (Event Hci.Packet.Event.Command_complete.v)

  let set_LE_scan_enable state duplicates =
    write (Command Hci.Packet.Command.LE.Set_scan_enable.v)
      { state; duplicates };
    expect (Event Hci.Packet.Event.Command_complete.v)

  let set_LE_scan_parameters type_ interval window own_address_type
      filter_policy =
    write (Command Hci.Packet.Command.LE.Set_scan_parameters.v)
      { type_; interval; window; own_address_type; filter_policy };
    expect (Event Hci.Packet.Event.Command_complete.v)

  let set_LE_advert_enable state =
    write (Command Hci.Packet.Command.LE.Set_advertise_enable.v) state;
    expect (Event Hci.Packet.Event.Command_complete.v)

  let set_LE_advert_parameters type_ interval_min interval_max own_address_type
      filter_policy =
    write (Command Hci.Packet.Command.LE.Set_advertising_parameters.v)
      { type_; interval_min; interval_max; own_address_type; filter_policy };
    expect (Event Hci.Packet.Event.Command_complete.v)

  (* https://github.com/google/eddystone/blob/master/protocol-specification.md *)
  let eddystone_frame = Bytes.of_string "\x10\x00\x03isometim.es"

  let set_LE_advert_data () =
    let data =
      Hci.Advert_data.
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
    write (Command Hci.Packet.Command.LE.Set_advertising_data.v) data;
    expect (Event Hci.Packet.Event.Command_complete.v)

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

  let set_event_filter filter =
    write (Command Hci.Packet.Command.Host_control.Set_event_filter.v) filter;
    expect (Event Hci.Packet.Event.Command_complete.v)

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
    |> ignore;
    set_LE_advert_data () |> ignore;
    set_LE_advert_enable 1 |> ignore

  let connect addr =
    write (Command Hci.Packet.Command.LE.Create_connection.v) addr;
    expect (Event Hci.Packet.Event.Command_complete.v)

  let bt_wait_for_connection () =
    let conn = ref None in
    while Option.is_none !conn do
      match Hci.wait ~get_byte:UART.read_byte with
      | Event (LE_meta_event { data })
        when Cstruct.get_uint8 data 0 = LE.Events.Connection_complete.id ->
          let data =
            LE.Events.Connection_complete.parse
              (Cstruct.sub data 1 (Cstruct.length data - 1))
          in
          Printf.printf "Connected to %S!\n%!" data.peer_address;
          conn := Some data
      | _ -> ()
    done;
    Option.get !conn

  let l2cap_write handle v =
    let size = L2cap.size v in
    let data = Cstruct.create_unsafe size in
    L2cap.write v data;
    write Acl { handle; data }

  let acl_respond_mtu handle mtu =
    let data = Cstruct.create_unsafe 3 in
    (* mtu response opcode *)
    Cstruct.set_uint8 data 0 0x03;
    Cstruct.LE.set_uint16 data 1 mtu;
    l2cap_write handle { channel = 4; data }

  let acl_respond_attribues_request handle =
    let data = Cstruct.create_unsafe 8 in
    (* attribues response opcode *)
    Cstruct.set_uint8 data 0 0x11;
    Cstruct.set_uint8 data 1 0x06;
    (* another length*)
    Cstruct.LE.set_uint16 data 2 0x0001;
    (* attribute handle*)
    Cstruct.LE.set_uint16 data 4 0xffff;
    (* end group handle *)
    Cstruct.LE.set_uint16 data 6 0xec00;
    l2cap_write handle { channel = 4; data }

  let acl_read_by_type_request handle =
    let data = Cstruct.create_unsafe 9 in
    (* attribues response opcode *)
    Cstruct.set_uint8 data 0 0x09;
    (* another length*)
    Cstruct.set_uint8 data 1 0x07;
    (* attribute handle*)
    Cstruct.LE.set_uint16 data 2 0x0001;
    (* attribute value *)
    (* FLAGS: write without response *)
    Cstruct.set_uint8 data 4 0x8;
    Cstruct.LE.set_uint16 data 5 0x0001;
    Cstruct.LE.set_uint16 data 7 0xec0e;
    l2cap_write handle { channel = 4; data }

  let acl_read_by_type_request_client_char_config handle =
    let data = Cstruct.create_unsafe 6 in
    (* attribues response opcode *)
    Cstruct.set_uint8 data 0 0x09;
    (* another length*)
    Cstruct.set_uint8 data 1 0x04;
    (* attribute handle*)
    Cstruct.LE.set_uint16 data 2 0x0001;
    (* attribute value *)
    Cstruct.LE.set_uint16 data 4 0x00;
    l2cap_write handle { channel = 4; data }

  let acl_error handle =
    let data = Cstruct.create_unsafe 5 in
    (* attribues response opcode *)
    Cstruct.set_uint8 data 0 0x01;
    (* request opcode in error *)
    Cstruct.set_uint8 data 1 0x08;
    (* attribute handle in error *)
    Cstruct.LE.set_uint16 data 2 0x0001;
    (* attribute_not_found error code *)
    Cstruct.set_uint8 data 4 0x0A;
    l2cap_write handle { channel = 4; data }

  let ack handle =
    let data = Cstruct.create 1 in
    (* attribues response opcode *)
    Cstruct.set_uint8 data 0 0x13;
    l2cap_write handle { channel = 4; data }

  let bt_wait_for_data () =
    let conn = ref None in
    while Option.is_none !conn do
      match Hci.wait ~get_byte:UART.read_byte with
      | Event (LE_meta_event { data }) ->
          Printf.printf "Unknown event %02x\n%!" (Cstruct.get_uint8 data 0)
      | Event (Unknown { id; _ }) -> Printf.printf "Ignored event %2x.\n%!" id
      | Event _ -> Printf.printf "Ignored known event.\n%!"
      | Acl { handle; data } ->
          let _length = Cstruct.LE.get_uint16 data 0 in
          let channel = Cstruct.LE.get_uint16 data 2 in
          if channel == 4 then
            let opcode = Cstruct.get_uint8 data 4 in
            if opcode == 2 then (
              let mtu = Cstruct.LE.get_uint16 data 5 in
              Printf.printf "MTU request: %d\n%!" mtu;
              acl_respond_mtu handle mtu)
            else if opcode == 0x10 then (
              let uuid = Cstruct.LE.get_uint16 data 9 in
              Printf.printf "attribues request: %2x\n%!" uuid;
              acl_respond_attribues_request handle)
            else if opcode == 0x08 then (
              let start = Cstruct.LE.get_uint16 data 5 in
              let uuid = Cstruct.LE.get_uint16 data 9 in
              Printf.printf "attribues type: %2x / %2x\n%!" start uuid;
              if start == 1 then
                if uuid == 0x2803 then acl_read_by_type_request handle
                else acl_read_by_type_request_client_char_config handle
              else acl_error handle)
            else if opcode == 0x12 then (
              let uuid = Cstruct.LE.get_uint16 data 5 in
              let data = Cstruct.to_string ~off:7 data in
              Printf.printf "Write %s to %02x\n%!" data uuid;
              ack handle)
    done;
    Option.get !conn
end
