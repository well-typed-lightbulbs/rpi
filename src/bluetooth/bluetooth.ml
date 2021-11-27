(* cstruct ? *)
module BA = Bigarray.Array1

module type UART = sig
  include Peripheral.S

  val read_byte : unit -> int

  val write_byte : int -> unit
end

module UART0 (Gpio : Rpi.Gpio.S) (Mtime : Rpi.Mtime.S) = struct
  include Peripheral.Make (struct
    let base = Mem.(Mmio.base + 0x201000n)

    let registers_size = 0x90n
  end)

  module Reg = struct
    module Dr = struct
      include Register.Make (struct
        let addr = 0x18n
      end)

      let data = int ~offset:0 ~size:8
    end

    module Fr = struct
      include Register.Make (struct
        let addr = 0x18n
      end)

      let receive_fifo_empty = bool ~offset:4

      let transmit_fifo_full = bool ~offset:5
    end

    module Imsc = struct
      include Register.Make (struct
        let addr = 0x38n
      end)
    end

    module Icr = struct
      include Register.Make (struct
        let addr = 0x44n
      end)
    end

    module Ibrd = struct
      include Register.Make (struct
        let addr = 0x24n
      end)
    end

    module Fbrd = struct
      include Register.Make (struct
        let addr = 0x28n
      end)
    end

    module Ifls = struct
      include Register.Make (struct
        let addr = 0x34n
      end)
    end

    module Lcrh = struct
      include Register.Make (struct
        let addr = 0x2cn
      end)
    end

    module Cr = struct
      include Register.Make (struct
        let addr = 0x30n
      end)
    end
  end

  let read_byte () = Reg.Dr.(read data)

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
    flushrx ();
    (* TODO: understand what's happening *)
    Mem.set_int Reg.Imsc.addr 0x00;
    Mem.set_int Reg.Icr.addr 0x7ff;
    Mem.set_int Reg.Ibrd.addr 0x1a;
    Mem.set_int Reg.Fbrd.addr 0x03;
    Mem.set_int Reg.Ifls.addr 0x08;
    Mem.set_int Reg.Lcrh.addr 0x70;
    Mem.set_int Reg.Cr.addr 0xB01;
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
    type ogf = Host_control | Le_control | Vendor

    type cmd =
      | Set_bdaddr
      | Reset_chip
      | Set_baud
      | Load_firmware
      | LE_scan_enable
      | LE_event_mask
      | LE_scan_parameters
      | LE_advert_enable
      | LE_advert_parameters
      | LE_advert_data
      | LE_connect

    let ogf_to_int = function
      | Host_control -> 0x03
      | Le_control -> 0x08
      | Vendor -> 0x3f

    let cmd_to_int = function
      | Set_bdaddr -> 0x01
      | Reset_chip -> 0x03
      | Set_baud -> 0x18
      | Load_firmware -> 0x2e
      | LE_event_mask -> 0x01
      | LE_scan_enable -> 0x0c
      | LE_scan_parameters -> 0x0b
      | LE_advert_enable -> 0x0a
      | LE_advert_parameters -> 0x06
      | LE_advert_data -> 0x08
      | LE_connect -> 0x0d

    let v ogf cmd =
      let opcode = (ogf_to_int ogf lsl 10) lor cmd_to_int cmd in
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

module Make (Mtime : Rpi.Mtime.S) (UART : UART) = struct
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
    assert (UART.read_byte () == Hci.Packet.event);

    match UART.read_byte () with
    | v when v = Hci.Event.connect_complete ->
        assert (UART.read_byte () <> 0);
        assert (UART.read_byte () == opcodebyte1);
        assert (UART.read_byte () == opcodebyte2)
    | v when v = Hci.Event.command_complete ->
        assert (UART.read_byte () == 4);
        assert (UART.read_byte () <> 0);
        assert (UART.read_byte () == opcodebyte1);
        assert (UART.read_byte () == opcodebyte2);
        assert (UART.read_byte () == 0)
    | _ -> assert false

  let hci_command ogf cmd = hci_command_bytes (Hci.Opcode.v ogf cmd)

  let empty = BA.create Int8_unsigned C_layout 0

  let bt_reset () = hci_command Host_control Reset_chip empty

  let bt_load_firmware () =
    hci_command Vendor Load_firmware empty;

    let data = bt_get_firmware () in
    let size = BA.size_in_bytes data in

    let rec send position =
      if position >= size then ()
      else
        let opcode0 = BA.unsafe_get data position in
        let opcode1 = BA.unsafe_get data (position + 1) in
        let length = BA.unsafe_get data (position + 2) in
        let slice = BA.sub data (position + 3) length in
        hci_command_bytes (opcode0, opcode1) slice;
        send (position + length + 3)
    in
    send 0;
    Mtime.sleep_us 100_000L

  let baudrate =
    (*  little endian, 115200 *)
    BA.of_array Int8_unsigned C_layout [| 0; 0; 0; 0xc2; 0x01; 0x00 |]

  let bt_setbaud () = hci_command Vendor Set_baud baudrate

  let bdaddr =
    BA.of_array Int8_unsigned C_layout [| 0xee; 0xff; 0xc0; 0xee; 0xff; 0xc0 |]

  let bt_setbdaddr () = hci_command Vendor Set_bdaddr bdaddr

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
    (* Set_bdaddr = 0x01: todo *)
    hci_command Le_control LE_event_mask
      (BA.of_array Int8_unsigned C_layout [| mask |])

  let set_LE_scan_enable state duplicates =
    (* 0x0c*)
    hci_command Le_control LE_scan_enable
      (BA.of_array Int8_unsigned C_layout [| state; duplicates |])

  let set_LE_scan_parameters type_ linterval hinterval lwindow hwindow
      own_address_type filter_policy =
    hci_command Le_control LE_scan_parameters
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
    hci_command Le_control LE_advert_enable
      (BA.of_array Int8_unsigned C_layout [| state |])

  let set_LE_advert_parameters type_ linterval_min hinterval_min linterval_max
      hinterval_max own_address_type filter_policy =
    hci_command Le_control LE_advert_parameters
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
    hci_command Le_control LE_advert_data data

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

    hci_command Le_control LE_connect command
end
