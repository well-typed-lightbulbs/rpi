let base = Mem.(Mmio.base + 0x20c000n)

let ctl = base

let sta = Mem.(base + 0x04n)

let rng1 = Mem.(base + 0x10n)

let dat1 = Mem.(base + 0x14n)

let fif1 = Mem.(base + 0x18n)

let rng2 = Mem.(base + 0x20n)

let dat2 = Mem.(base + 0x24n)

let init () =
  Gpio.(set_pull_state P18 PULL_OFF);
  Gpio.(set_func P18 F_ALT5);
  Clock.set_pwm_clock (3 * 800000);
  Mem.set_int ctl ((1 lsl 7) lor (1 lsl 5) lor (1 lsl 0))

let write int_val =
  while Mem.get_int sta land 1 == 1 do
    ()
  done;
  Mem.set_int fif1 int_val
