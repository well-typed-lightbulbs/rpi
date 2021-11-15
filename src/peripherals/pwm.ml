include Peripheral.Make (struct
  let base `Rpi4 = Mem.(Mmio.base + 0x20c000n)

  let registers_size = 0x28n
end)

let ctl base = base

let sta base = Mem.(base + 0x04n)

let rng1 base = Mem.(base + 0x10n)

let dat1 base = Mem.(base + 0x14n)

let fif1 base = Mem.(base + 0x18n)

let rng2 base = Mem.(base + 0x20n)

let dat2 base = Mem.(base + 0x24n)

let init ~clock ~gpio base =
  Gpio.(set_pull_state gpio P18 PULL_OFF);
  Gpio.(set_func gpio P18 F_ALT5);
  Clock.set_pwm_clock clock (3 * 800000);
  Mem.set_int (ctl base) ((1 lsl 7) lor (1 lsl 5) lor (1 lsl 0))

let write base int_val =
  while Mem.get_int (sta base) land 1 == 1 do
    ()
  done;
  Mem.set_int (fif1 base) int_val
