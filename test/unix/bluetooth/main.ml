module Mtime = Rpi.Mtime.Make (Rpi_unix.Make (Rpi.Mtime))
module Clock = Rpi.Clock.Make (Rpi_unix.Make (Rpi.Clock)) (Mtime)
module Gpio = Rpi.Gpio.Make (Rpi_unix.Make (Rpi.Gpio))
module UART =
  Bluetooth.UART0.Make (Gpio) (Mtime) (Rpi_unix.Make (Bluetooth.UART0))
module Bluetooth = Bluetooth.Make (Mtime) (UART)

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
