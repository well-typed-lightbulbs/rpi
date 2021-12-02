module UART = Bluetooth.UART0
module Bluetooth = Bluetooth.Make (UART)

let () =
  Printf.printf "INIT\n%!";
  UART.init ();
  Printf.printf "BT RESET\n%!";
  Bluetooth.bt_reset ();
  Printf.printf "BT FIRMWARE\n%!";
  Bluetooth.bt_load_firmware ();
  Printf.printf "BT SET BAUD\n%!";
  Bluetooth.bt_setbaud ();
  Printf.printf "BT SET BDADDR\n%!";
  Bluetooth.bt_setbdaddr ();
  Printf.printf "BT GET BDADDR\n%!";
  Bluetooth.bt_getbdaddr () |> Array.iter (Printf.printf "%2x");
  Printf.printf "\n%!"
