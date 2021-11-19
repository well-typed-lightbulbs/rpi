let base `Rpi4 = Mem.(Mmio.base + 0x20c000n)

let registers_size = 0x28n

module Make (*

  (Gpio: Gpio.S)
  (Clock: Clock.S)
  (Mtime: Mtime.S)
*)
  (B: sig val base : Mem.addr end) = struct

  module Reg = struct
    
    module Ctl = struct
      include Register.Make(struct let addr = B.base end)

      let msen1 = bool ~offset:7

      let clrf = bool ~offset:6

      let usef1 = bool ~offset:5

      let mode1 = bool ~offset:1

      let pwen1 = bool ~offset:0

    end

    module Sta = struct
      include Register.Make(struct let addr = Mem.(B.base + 0x04n) end)
      
      let empt1 = bool ~offset:1

      let full1 = bool ~offset:1

    end

    let rng1 = Mem.(B.base + 0x10n)

  end



  let stop () = 
    while not Reg.Sta.(read empt1) do
      ()
    done;
    Reg.Sta.(write empty)

  let init () =
    Gpio.(set_pull_state gpio P18 PULL_DOWN);
    Gpio.(set_func gpio P18 F_ALT5);

    stop ();
    Clock.kill ~mtime clock;
    Clock.set_pwm_clock ~mtime clock (3 * 800000);

    
    Mem.set_int Reg.rng1 32;
    Reg.Ctl.(empty |> set clrf true |> write);
    Reg.Ctl.(empty |> set msen1 true |>set usef1 true |> set mode1 true |> write);
    Mtime.sleep_us 10L;
    Reg.Ctl.(empty |> set msen1 true |>set usef1 true |> set mode1 true |> set pwen1 true |> write);

end













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


let (lsl) = Int32.shift_left

let (land) = Int32.logand

let (lor) = Int32.logor

let (/) = Int32.div

let stop base = 
  while Mem.get_int32 (sta base) land (1l lsl 1) = 0l do
    ()
  done;
  Mem.set_int32 (ctl base) 0l

let init ~mtime ~clock ~gpio base =
  Gpio.(set_pull_state gpio P18 PULL_DOWN);
  Gpio.(set_func gpio P18 F_ALT5);

  stop base;
  Clock.kill ~mtime clock;

  Clock.set_pwm_clock ~mtime clock (3 * 800000);
  Mem.set_int32 (rng1 base) 32l;
  Mem.set_int32 (ctl base) (1l lsl 6);
  Mem.set_int32 (ctl base) ((1l lsl 7) lor (1l lsl 5) lor (1l lsl 1));
  Mtime.sleep_us mtime 10L;
  Mem.set_int32 (ctl base) ((1l lsl 7) lor (1l lsl 5) lor (1l lsl 1) lor (1l lsl 0))


let write base int_val = 
  let sta = sta base in
  let fif1 = fif1 base in
  while Stdlib.(Mem.get_int sta land 1 == 1) do
    ()
  done;
  Mem.set_int32 fif1 int_val
