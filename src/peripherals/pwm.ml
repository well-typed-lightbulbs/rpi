let base = Rpi_hardware.pwm

module Reg = struct
  module Ctl = struct
    include Register.Make (struct
      let addr = base
    end)

    let msen1 = bool ~offset:7

    let clrf = bool ~offset:6

    let usef1 = bool ~offset:5

    let mode1 = bool ~offset:1

    let pwen1 = bool ~offset:0
  end

  module Sta = struct
    include Register.Make (struct
      let addr = Mem.(base + 0x04n)
    end)

    let empt1 = bool ~offset:1

    let full1 = bool ~offset:0
  end

  let rng1 = Mem.(base + 0x10n)
end

let stop () =
  while not Reg.Sta.(read empt1) do
    ()
  done;
  Reg.Ctl.(write empty)

let init () =
  Mem.dmb ();

  (* ? -> GPIO *)
  Gpio.(set_pull_state P18 PULL_DOWN);
  Gpio.(set_func P18 F_ALT5);

  Mem.dmb ();

  (* GPIO -> PWM *)
  stop ();

  Mem.dmb ();
  (* PWM -> CLOCK *)
  Mtime.sleep_us 10L;
  Clock.kill ();
  Mtime.sleep_us 10L;
  Clock.set_pwm_clock (3 * 800000);

  Mem.dmb ();

  (* CLOCK -> PWM *)
  Mem.set_int Reg.rng1 32;
  Reg.Ctl.(empty |> set clrf true |> write);
  Mtime.sleep_us 10L;
  Reg.Ctl.(empty |> set msen1 true |> set usef1 true |> set mode1 true |> write);
  Mtime.sleep_us 10L;
  Reg.Ctl.(
    empty |> set msen1 true |> set usef1 true |> set mode1 true
    |> set pwen1 true |> write)

let fif1 = Mem.(base + 0x18n)

let write int_val =
  while Reg.Sta.(read full1) do
    ()
  done;
  Mem.set_int fif1 int_val
