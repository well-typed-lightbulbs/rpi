module UART = Bluetooth.UART0
module Bluetooth = Bluetooth.Make (UART)

let print_bdaddr () =
  Bluetooth.bt_getbdaddr ()
  |> String.iter (fun c -> Char.code c |> Printf.printf "%02x");
  Printf.printf "\n%!"

let setup () =
  Printf.printf "INIT\n%!";
  UART.init ();
  Printf.printf "BT RESET\n%!";
  Bluetooth.bt_reset () |> ignore;
  Printf.printf "BT FIRMWARE\n%!";
  Bluetooth.bt_load_firmware ();
  Printf.printf "BT SET BAUD\n%!";
  Bluetooth.bt_setbaud () |> ignore;
  Printf.printf "BT SET BDADDR\n%!";
  Bluetooth.bt_setbdaddr () |> ignore

let init_file = "/tmp/bt-is-up"

let maybe_init () =
  try
    let fd = Unix.openfile init_file [ Unix.O_RDONLY ] 0o660 in
    Printf.printf
      "[INFO] Bluetooth is already set up. To reset, remove the file '%s'.\n%!"
      init_file;
    Unix.close fd
  with Unix.Unix_error (Unix.ENOENT, _, _) ->
    Printf.printf "Setting up bluetooth.\n%!";
    setup ();
    let fd = Unix.openfile init_file [ Unix.O_CREAT; O_WRONLY ] 0o660 in
    Unix.close fd

let () =
  Sys.(
    set_signal sigint
      (Signal_handle
         (fun _ ->
           Printf.printf "Exiting.\n%!";
           Bluetooth.stop_scanning () |> ignore;
           Bluetooth.stop_advertising () |> ignore;
           Unix._exit 0)))

let () =
  UART.flushrx ();
  maybe_init ();
  Printf.printf "Hi.\n%!";
  print_bdaddr ();
  Printf.printf "Set event mask.\n%!";
  Bluetooth.set_event_mask
    {
      le_meta = true;
      inquiry_result = true;
      inquiry_complete = true;
      connection_complete = true;
      disconnection_complete = true;
      authentication_complete = true;
      remote_name_request_complete = true;
      connection_request = true;
    }
  |> ignore;
  Printf.printf "Set event filter.\n%!";
  Bluetooth.set_event_filter (Connection_setup (Allow_all Off)) |> ignore;
  Bluetooth.set_LE_event_mask 0xff |> ignore;
  Bluetooth.start_active_advertising ();
  Printf.printf "Scanning..\n%!";
  Bluetooth.bt_wait_for_connection () |> ignore;
  Printf.printf ".\n%!";
  while true do
    Bluetooth.bt_wait_for_data () |> ignore
  done
