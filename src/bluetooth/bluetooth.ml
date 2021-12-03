open Rpi

(* some docs:
   http://software-dl.ti.com/simplelink/esd/simplelink_cc13x2_sdk/1.60.00.29_new/exports/docs/ble5stack/vendor_specific_guide/BLE_Vendor_Specific_HCI_Guide/hci_interface.html
*)

(* cstruct ? *)
module BA = Bigarray.Array1

module type UART = sig
  val read_byte : unit -> int

  val read_byte_ready : unit -> bool

  val write_byte : int -> unit
end

module UART0 = struct
  let base = Rpi_hardware.uart0

  module Reg = struct
    module Dr = struct
      include Register.Make (struct
        let addr = base
      end)

      let data = int ~offset:0 ~size:8
    end

    module Fr = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x18n)
      end)

      let receive_fifo_empty = bool ~offset:4

      let transmit_fifo_full = bool ~offset:5
    end

    module Imsc = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x38n)
      end)
    end

    module Icr = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x44n)
      end)
    end

    module Ibrd = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x24n)
      end)
    end

    module Fbrd = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x28n)
      end)
    end

    module Ifls = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x34n)
      end)
    end

    module Lcrh = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x2cn)
      end)
    end

    module Cr = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x30n)
      end)
    end
  end

  let read_byte_ready () = not Reg.Fr.(read receive_fifo_empty)

  let read_byte () =
    while Reg.Fr.(read receive_fifo_empty) do
      ()
    done;
    Reg.Dr.(read data)

  let flushrx () =
    while not Reg.Fr.(read receive_fifo_empty) do
      read_byte () |> ignore
    done

  let write_byte byte =
    while Reg.Fr.(read transmit_fifo_full) do
      ()
    done;
    Reg.Dr.(empty |> set data byte |> write)

  let init () =
    (* UART SET UP*)
    Gpio.set_func P30 F_ALT3;
    Gpio.set_func P31 F_ALT3;
    Gpio.set_func P32 F_ALT3;
    Gpio.set_func P33 F_ALT3;
    Gpio.set_func P14 F_ALT5;
    Gpio.set_func P15 F_ALT5;
    flushrx ();
    (* TODO: understand what's happening *)
    Mem.set_int Reg.Imsc.addr 0x00;
    Mem.set_int Reg.Icr.addr 0x7ff;
    Mem.set_int Reg.Ibrd.addr 0x1a;
    Mem.set_int Reg.Fbrd.addr 0x03;
    Mem.set_int Reg.Ifls.addr 0x08;
    Mem.set_int Reg.Lcrh.addr 0x70;
    Mem.set_int Reg.Cr.addr 0xB01;
    Printf.printf "UART0 READY\n%!";
    Mem.set_int Reg.Imsc.addr 0x430;
    Mtime.sleep_us 10_000L
end

module Hci = struct
  module Packet = struct
    let command = 0x01

    let acl = 0x02

    let event = 0x04
  end

  module Event = struct
    let command_complete = 0x0e

    let connect_complete = 0x0f
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
        | Read_transmit_power_level [@id 45]
        | Set_control_to_host_flow_control [@id 49]
        | Host_buffer_size [@id 51]
        | Host_number_of_completed_packets [@id 53]
        | Set_event_mask_page [@id 63]
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

    let v cmd =
      let ogf, ogc =
        match cmd with
        | Host_control hc -> (Host_control.ogf, Host_control.t_to_int hc)
        | LE_control hc -> (LE.ogf, LE.t_to_int hc)
        | Vendor hc -> (Vendor.ogf, Vendor.t_to_int hc)
      in
      let opcode = (ogf lsl 10) lor Int32.to_int ogc in
      let b_hi = (opcode lsr 8) land 0xff in
      let b_lo = opcode land 0xff in
      (b_lo, b_hi)
  end

  module LL = struct
    let scan_active = 0x01

    let adv_nonconn_ind = 0x03
  end
end

external bt_get_firmware :
  unit -> (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) BA.t
  = "caml_bt_get_firmware"

module Make (UART : UART) = struct
  let uart_assert_read value =
    let value' = UART.read_byte () in
    if value' != value then (
      Printf.printf "BT assertion failed: got %02x instead of %02x\n%!" value'
        value;
      assert false)

  let hci_command_bytes (opcodebyte1, opcodebyte2) data =
    let length = BA.size_in_bytes data in
    UART.write_byte Hci.Packet.command;
    UART.write_byte opcodebyte1;
    UART.write_byte opcodebyte2;
    UART.write_byte length;
    for i = 0 to length - 1 do
      let byte = BA.unsafe_get data i in
      UART.write_byte byte
    done;
    uart_assert_read Hci.Packet.event;

    match UART.read_byte () with
    | v when v = Hci.Event.connect_complete ->
        assert (UART.read_byte () <> 0);
        uart_assert_read opcodebyte1;
        uart_assert_read opcodebyte2
    | v when v = Hci.Event.command_complete ->
        uart_assert_read 4;
        assert (UART.read_byte () <> 0);
        uart_assert_read opcodebyte1;
        uart_assert_read opcodebyte2;
        uart_assert_read 0
    | _ -> assert false

  let hci_command cmd = hci_command_bytes (Hci.Opcode.v cmd)

  let empty = BA.create Int8_unsigned C_layout 0

  let bt_reset () = hci_command Hci.Opcode.(Host_control Reset) empty

  let bt_load_firmware () =
    let data = bt_get_firmware () in
    let size = BA.size_in_bytes data in
    assert (size < 100000);
    hci_command Hci.Opcode.(Vendor Load_firmware) empty;

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
        let opcode0 = BA.unsafe_get data position in
        let opcode1 = BA.unsafe_get data (position + 1) in
        let length = BA.unsafe_get data (position + 2) in
        let slice = BA.sub data (position + 3) length in
        hci_command_bytes (opcode0, opcode1) slice;
        send (position + length + 3))
    in
    send 0;
    Printf.printf "Success: waiting for a sec.\n%!";
    Mtime.sleep_us 1_000_000L

  let baudrate =
    (*  little endian, 115200 *)
    BA.of_array Int8_unsigned C_layout [| 0; 0; 0; 0xc2; 0x01; 0x00 |]

  let bt_setbaud () = hci_command Hci.Opcode.(Vendor Set_baud) baudrate

  let bdaddr =
    BA.of_array Int8_unsigned C_layout [| 0xee; 0xff; 0xc0; 0xee; 0xff; 0xc0 |]

  let bt_setbdaddr () = hci_command Hci.Opcode.(Vendor Set_bdaddr) bdaddr

  let bt_getbdaddr () =
    UART.write_byte Hci.Packet.command;
    UART.write_byte 0x09;
    UART.write_byte 0x10;
    UART.write_byte 0x00;

    assert (UART.read_byte () == Hci.Packet.event);
    assert (UART.read_byte () == Hci.Event.command_complete);
    assert (UART.read_byte () == 0x0a);
    assert (UART.read_byte () == 1);
    assert (UART.read_byte () == 0x09);
    assert (UART.read_byte () == 0x10);
    assert (UART.read_byte () == 0x00);

    let data = Array.make 6 0 in
    for i = 0 to 5 do
      data.(i) <- UART.read_byte ()
    done;
    data

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

  let set_LE_event_mask mask =
    let command = BA.create Int8_unsigned C_layout 8 in
    command.{0} <- mask;
    hci_command Hci.Opcode.(LE_control Set_event_mask) command

  let set_LE_scan_enable state duplicates =
    (* 0x0c*)
    hci_command
      Hci.Opcode.(LE_control Set_scan_enable)
      (BA.of_array Int8_unsigned C_layout [| state; duplicates |])

  let set_LE_scan_parameters type_ linterval hinterval lwindow hwindow
      own_address_type filter_policy =
    hci_command
      Hci.Opcode.(LE_control Set_scan_parameters)
      (BA.of_array Int8_unsigned C_layout
         [|
           type_;
           linterval;
           hinterval;
           lwindow;
           hwindow;
           own_address_type;
           filter_policy;
         |])

  let set_LE_advert_enable state =
    hci_command
      Hci.Opcode.(LE_control Set_advertise_enable)
      (BA.of_array Int8_unsigned C_layout [| state |])

  let set_LE_advert_parameters type_ linterval_min hinterval_min linterval_max
      hinterval_max own_address_type filter_policy =
    hci_command
      Hci.Opcode.(LE_control Set_advertising_parameters)
      (BA.of_array Int8_unsigned C_layout
         [|
           linterval_min;
           hinterval_min;
           linterval_max;
           hinterval_max;
           type_;
           own_address_type;
           (* 5 *)
           0;
           0;
           0;
           0;
           0;
           0;
           0;
           0x07;
           (* 13*)
           filter_policy;
         |])

  let advert_data =
    [| 
    0x19;
    0x02; 0x01; 0x06;
    0x03; 0x03; 0xAA; 0xFE;
    0x11; 0x16; 0xAA; 0xFE; 0x10; 0x00; 0x03;
    0x69; 0x73; 0x6f; 0x6d; 0x65; 0x74; 0x69; 0x6d;
    0x2e; 0x65; 0x73;
    0; 0; 0; 0; 0; 0 
    |] [@ocamlformat "disable"]

  let set_LE_advert_data () =
    let data = BA.of_array Int8_unsigned C_layout advert_data in
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
    set_LE_scan_parameters Hci.LL.scan_active (lo p) (hi p) (lo q) (hi q) 0 0

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
    set_LE_advert_parameters Hci.LL.adv_nonconn_ind (lo min_interval)
      (hi min_interval) (lo max_interval) (hi max_interval) 0 0;
    set_LE_advert_data ();
    set_LE_advert_enable 1

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
    let command = BA.create Int8_unsigned C_layout 25 in
    command.{0} <- lo p;
    command.{2} <- lo q;
    command.{6} <- addr.(5);
    command.{7} <- addr.(4);
    command.{8} <- addr.(3);
    command.{9} <- addr.(2);
    command.{10} <- addr.(1);
    command.{11} <- addr.(0);
    command.{13} <- lo min_interval;
    command.{15} <- lo max_interval;
    command.{19} <- 0x2a;
    command.{20} <- 0x00;

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

  let hci_state =
    {
      data_buf = Bytes.empty;
      data_len = 0;
      poll_state = 0;
      messages_received = 0;
    }

  let max_msg_len = 50

  let hci_poll2 byte =
    match hci_state.poll_state with
    | 0 ->
        if byte != hci_event_pkt then hci_state.poll_state <- 0
        else hci_state.poll_state <- 1
    | 1 ->
        if byte != le_event_code then hci_state.poll_state <- 0
        else hci_state.poll_state <- 2
    | 2 ->
        if byte > max_msg_len then hci_state.poll_state <- 0
        else (
          hci_state.poll_state <- 3;
          hci_state.data_len <- byte;
          hci_state.data_buf <- Bytes.make byte '\000')
    | _ ->
        hci_state.data_buf.[hci_state.poll_state - 3] <- Char.chr byte;
        if hci_state.poll_state = hci_state.data_len + 3 - 1 then (
          hci_state.messages_received <- hci_state.messages_received + 1;
          hci_state.poll_state <- 0)
        else hci_state.poll_state <- hci_state.poll_state + 1

  let max_read_run = 100

  let hci_poll () =
    let goal = hci_state.messages_received + 1 in
    if UART.read_byte_ready () then
      let rec loop i =
        if i > max_read_run || not (UART.read_byte_ready ()) then None
        else (
          hci_poll2 (UART.read_byte ());
          if hci_state.messages_received = goal then Some hci_state.data_buf
          else loop (i + 1))
      in
      loop 0
    else None

  
    type connect_state = {
      got_echo_sid : bool;
      got_echo_name : bool;
      echo_addr : string;
      connected : bool;
      connection_handle : int;
    }

  let bt_search () =
    while true do
      match hci_poll () with 
      | None -> ()
      | Some buf -> 
        if Bytes.get buf 0 = (Char.chr le_adreport_code) then
          if Bytes.get buf 1 = Char.chr 1 then
            if Bytes.get buf 0 = Char.chr 0 then 
              let ad_len = Bytes.get buf 11 in
              ()
    done


    


    

 
let bt_conn () = 
  while true do
    match hci_poll () with 
    | None  -> ()
    | Some buf ->
      if !connect_state.connected && hci_state.data_len >=2 && (Bytes.get )

  
end
