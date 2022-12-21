(* https://www.scribd.com/doc/127599939/BCM2835-Audio-clocks *)

let base = Rpi_hardware.clock
let crystal_frequency = Rpi_hardware.crystal_frequency

module Reg = struct
  open Register

  module Cm_pwmdiv = struct
    include Register.Make (struct
      let addr = Mem.(offset base 0xa4)
    end)

    let password =
      {
        offset = 24;
        size = 8;
        to_int = (fun () -> 0x5a);
        of_int = (fun _ -> ());
      }

    let divi = int ~size:12 ~offset:12
  end

  module Cm_pwmctl = struct
    include Register.Make (struct
      let addr = Mem.(offset base 0xa0)
    end)

    let password =
      {
        offset = 24;
        size = 8;
        to_int = (fun () -> 0x5a);
        of_int = (fun _ -> ());
      }

    let busy = bool ~offset:7
    let enable = bool ~offset:4
    let kill = bool ~offset:5

    type source = GND | OSC | DBG0 | DBG1 | PLLA | PLLC | PLLD | HDMI_AUX

    let source_to_int = function
      | GND -> 0
      | OSC -> 1
      | DBG0 -> 2
      | DBG1 -> 3
      | PLLA -> 4
      | PLLC -> 5
      | PLLD -> 6
      | HDMI_AUX -> 7

    let source_of_int = function
      | 0 -> GND
      | 1 -> OSC
      | 2 -> DBG0
      | 3 -> DBG1
      | 4 -> PLLA
      | 5 -> PLLC
      | 6 -> PLLD
      | 7 -> HDMI_AUX
      | _ -> GND

    let source =
      { offset = 0; size = 4; to_int = source_to_int; of_int = source_of_int }
  end
end

let wait_while_busy () =
  while Reg.Cm_pwmctl.(read () && busy) do
    ()
  done

let wait_until_busy () =
  while not Reg.Cm_pwmctl.(read () && busy) do
    ()
  done

let clock_freq = ref None
let freq () = !clock_freq

let set_pwm_clock freq =
  match !clock_freq with
  | Some v -> Error v
  | None ->
      Mem.dmb ();
      Reg.Cm_pwmdiv.(
        empty |> set password () |> set divi (crystal_frequency / freq) |> write);
      Reg.Cm_pwmctl.(empty |> set password () |> write);
      Reg.Cm_pwmctl.(empty |> set password () |> set source OSC |> write);
      Mtime.sleep_us 10L;
      Reg.Cm_pwmctl.(
        empty |> set password () |> set source OSC |> set enable true |> write);
      wait_until_busy ();
      clock_freq := Some freq;
      Ok ()

let kill () =
  Reg.Cm_pwmctl.(empty |> set password () |> write);
  (* set zero *)
  Mtime.sleep_us 10L;
  Reg.Cm_pwmctl.(empty |> set password () |> set kill true |> write);
  (* set kill*)
  wait_while_busy ()
