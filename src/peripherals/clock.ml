include Peripheral.Make (struct
  let base `Rpi4 = Mem.(Mmio.base + 0x101000n)

  let registers_size = 0xb0n
end)

let cm_pwmdiv base = Mem.(base + 0xa4n)

let cm_pwmctl base = Mem.(base + 0xa0n)

let (lsl) = Int32.shift_left

let (land) = Int32.logand

let (lor) = Int32.logor

let (/) = Int32.div

let password = 0x5al lsl 24

let pi4_freq = 54000000l


let div_divi value = (value land 0xfffl) lsl 12

let wait_while_busy base=
while Mem.get_int32 (cm_pwmctl base) land (1l lsl 7) <> 0l do
  ()
done

let wait_until_busy base=
while Mem.get_int32 (cm_pwmctl base) land (1l lsl 7) = 0l do
  ()
done

let with_password = 
  Int32.logor password

let set_pwm_clock ~mtime base freq =
  Mem.(set_int32 (cm_pwmdiv base) (with_password (div_divi (pi4_freq / (Int32.of_int freq)))));
  Mem.(set_int32 (cm_pwmctl base) (with_password 1l));
  Mtime.sleep_us mtime 10L;
  Mem.(set_int32 (cm_pwmctl base) (with_password 1l lor (1l lsl 4)));
  wait_until_busy base

let kill ~mtime base =
  Mem.(set_int32 (cm_pwmctl base) password); (* set zero *)
  Mtime.sleep_us mtime 10L;
  Mem.(set_int32 (cm_pwmctl base) (password lor (1l lsl 5))); (* set kill*)
  wait_while_busy base;


