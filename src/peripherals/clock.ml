(* https://www.scribd.com/doc/127599939/BCM2835-Audio-clocks *)

include Peripheral.Make (struct
  let base = Mem.(Mmio.base + 0x101000n)

  let registers_size = 0xb0n
end)

module type S = sig
  val set_pwm_clock : int -> unit

  val kill : unit -> unit
end

module Make (Mtime : Mtime.S) (B : Base) = struct
  module Reg = struct
    module Cm_pwmdiv = struct
      include Register.Make (struct
        let addr = Mem.(B.base + 0xa4n)
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
        let addr = Mem.(B.base + 0xa0n)
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

  let cm_pwmdiv = Mem.(B.base + 0xa4n)

  let cm_pwmctl = Mem.(B.base + 0xa0n)
  (*
  let password = 0x5a lsl 24*)

  let pi4_freq = 54000000

  (*  let div_divi value = (value land 0xfff) lsl 12*)

  let wait_while_busy () =
    while Reg.Cm_pwmctl.(read busy) do
      ()
    done

  let wait_until_busy () =
    while not Reg.Cm_pwmctl.(read busy) do
      ()
    done

  let set_pwm_clock freq =
    Mem.dmb ();
    Reg.Cm_pwmdiv.(
      empty |> set password () |> set divi (pi4_freq / freq) |> write);
    Reg.Cm_pwmctl.(empty |> set password () |> write);
    Reg.Cm_pwmctl.(empty |> set password () |> set source OSC |> write);
    Mtime.sleep_us 10L;
    Reg.Cm_pwmctl.(
      empty |> set password () |> set source OSC |> set enable true |> write);
    wait_until_busy ()

  let password = 0x5a lsl 24

  let kill () =
    Reg.Cm_pwmctl.(empty |> set password () |> write);
    (* set zero *)
    Mtime.sleep_us 10L;
    Reg.Cm_pwmctl.(empty |> set password () |> set kill true |> write);
    (* set kill*)
    wait_while_busy ()
end
