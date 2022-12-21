module UART = Uart_poll.UART0
module Bluetooth = Bluetooth.Make (UART)
open Lwt.Syntax

let print_bdaddr () =
  let+ addr = Bluetooth.bt_getbdaddr () in
  addr |> String.iter (fun c -> Char.code c |> Printf.printf "%02x");
  Printf.printf "\n%!"

let setup () =
  Printf.printf "INIT\n%!";
  UART.init ();
  Printf.printf "BT RESET\n%!";
  let* _ = Bluetooth.bt_reset () in
  Printf.printf "BT FIRMWARE\n%!";
  let* () = Bluetooth.bt_load_firmware () in
  Printf.printf "BT SET BAUD\n%!";
  let* _ = Bluetooth.bt_setbaud () in
  Printf.printf "BT SET BDADDR\n%!";
  Bluetooth.bt_setbdaddr ()

let init_file = "/tmp/bt-is-up"

let maybe_init () =
  try
    let fd = Unix.openfile init_file [ Unix.O_RDONLY ] 0o660 in
    Printf.printf
      "[INFO] Bluetooth is already set up. To reset, remove the file '%s'.\n%!"
      init_file;
    Unix.close fd;
    Lwt.return_unit
  with Unix.Unix_error (Unix.ENOENT, _, _) ->
    Printf.printf "Setting up bluetooth.\n%!";
    let+ _ = setup () in
    let fd = Unix.openfile init_file [ Unix.O_CREAT; O_WRONLY ] 0o660 in
    Unix.close fd

let () =
  Sys.(
    set_signal sigint
      (Signal_handle
         (fun _ ->
           Printf.printf "Exiting.\n%!";
           (* Bluetooth.stop_scanning () |> ignore;
           Bluetooth.stop_advertising () |> ignore; *)
           Unix._exit 0)))

let () =
  Lwt_main.run
  @@
  (Rpi.UART.UART0.flushrx ();
   let* () = maybe_init () in
   Printf.printf "Hi.\n%!";
   let* () = print_bdaddr () in
   Lwt.return_unit)
   (* Printf.printf "Set event mask.\n%!";
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
   let+ _ = Bluetooth.start_active_advertising () in
   Printf.printf "Scanning..\n%!";
   Bluetooth.bt_wait_for_connection () |> ignore;
   Printf.printf ".\n%!";
   while true do
     Bluetooth.bt_wait_for_data () |> ignore
   done) *)
