let base  = Mem.(Mmio.base + 0x101000n)

let registers_size = 0xb0n


module type S = sig 
  val set_pwm_clock : int -> unit

  val kill : unit -> unit
end

module Make(Mtime: Mtime.S)(B: Peripheral.Base) = struct
  
  let cm_pwmdiv = Mem.(B.base + 0xa4n)

  let cm_pwmctl = Mem.(B.base + 0xa0n)

  let password = 0x5a lsl 24

  let pi4_freq = 54000000


  let div_divi value = (value land 0xfff) lsl 12

  let wait_while_busy () =
  while Mem.get_int (cm_pwmctl ) land (1 lsl 7) <> 0 do
    ()
  done

  let wait_until_busy () =
  while Mem.get_int (cm_pwmctl ) land (1 lsl 7) = 0 do
    ()
  done

  let with_password v = password lor v

  let set_pwm_clock freq =
    Mem.(set_int (cm_pwmdiv) (with_password (div_divi (pi4_freq / freq))));
    Mem.(set_int (cm_pwmctl) (with_password 1));
    Mtime.sleep_us 10L;
    Mem.(set_int (cm_pwmctl) (with_password 1 lor (1 lsl 4)));
    wait_until_busy ()

  let kill () =
    Mem.(set_int (cm_pwmctl) password); (* set zero *)
    Mtime.sleep_us 10L;
    Mem.(set_int (cm_pwmctl) (password lor (1 lsl 5))); (* set kill*)
    wait_while_busy ();

end
