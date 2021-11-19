
(*

module type CtlRegister = sig 

      type 'a field

      val msen1 : bool field

      val usef1 : bool field

      val mode1 : bool field

      val pwen1 : bool field

      val read : 'a field -> 'a

      type writer

      val empty : writer

      val set : 'a field -> 'a -> writer -> writer 

      val write : writer

    end

module Make(B: sig val base : Mem.addr end) = struct

  module Reg = struct
    
    module Ctl = struct

      let addr = B.base

      type 'a field = {
        offset: int;
        size: int;
        to_int: 'a -> int32;
        of_int: int32 -> 'a;
      }

      let msen1 = {
        offset=7;
        size=1;
        to_int=(fun x -> if x then 1l else 0l);
        of_int=(fun x -> x == 1l);
      }


      let read {offset; size; of_int; _} =

        (* XXbbbXX
             XXbbb
             & 111
               bbb
                
         *)

        (Mem.get_int addr lsr offset) land ((1 lsl size - 1)) == 1





      
    end

  end







end
*)












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
