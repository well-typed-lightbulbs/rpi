let password = 0x5a lsl 24

let pi4_freq = 54000000

let div_divi value = (value land 0xfff) lsl 12

let base = Mem.(Mmio.base + 0x101000n)

let cm_pwmdiv = Mem.(base + 0xa4n)

let cm_pwmctl = Mem.(base + 0xa0n)

let set_pwm_clock freq =
  Mem.(set_int cm_pwmdiv (password lor div_divi (pi4_freq / freq)));
  Mem.(set_int cm_pwmctl (password lor 1));
  Mem.(set_int cm_pwmctl (password lor 1 lor (1 lsl 4)));
  (* TODO Fix this*)
  (* Mtime.sleep_us 10L; *)
  while Mem.get_int cm_pwmctl land (1 lsl 7) <> 0 do
    ()
  done
