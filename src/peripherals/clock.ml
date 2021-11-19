include Peripheral.Make (struct
  let base `Rpi4 = Mem.(Mmio.base + 0x101000n)

  let registers_size = 0xb0n
end)

let cm_pwmdiv base = Mem.(base + 0xa4n)

let cm_pwmctl base = Mem.(base + 0xa0n)

let password = 0x5a lsl 24

let pi4_freq = 54000000


let div_divi value = (value land 0xfff) lsl 12

let wait_while_busy base=
while Mem.get_int (cm_pwmctl base) land (1 lsl 7) <> 0 do
  ()
done

let wait_until_busy base=
while Mem.get_int (cm_pwmctl base) land (1 lsl 7) = 0 do
  ()
done

let with_password v = password lor v

let set_pwm_clock ~mtime base freq =
  Mem.(set_int (cm_pwmdiv base) (with_password (div_divi (pi4_freq / freq))));
  Mem.(set_int (cm_pwmctl base) (with_password 1));
  Mtime.sleep_us mtime 10L;
  Mem.(set_int (cm_pwmctl base) (with_password 1 lor (1 lsl 4)));
  wait_until_busy base

let kill ~mtime base =
  Mem.(set_int (cm_pwmctl base) password); (* set zero *)
  Mtime.sleep_us mtime 10L;
  Mem.(set_int (cm_pwmctl base) (password lor (1 lsl 5))); (* set kill*)
  wait_while_busy base;


