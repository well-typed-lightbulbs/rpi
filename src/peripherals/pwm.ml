let base  = Mem.(Mmio.base + 0x20c000n)

let registers_size = 0x28n


module type S = sig 
  val init : unit -> unit

  val write : int -> unit

  val stop : unit -> unit
end


module Make 
  (Gpio: Gpio.S)
  (Clock: Clock.S)
  (Mtime: Mtime.S)
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

      let full1 = bool ~offset:0

    end

    let rng1 = Mem.(B.base + 0x10n)

  end



  let stop () = 
    while not Reg.Sta.(read empt1) do
      ()
    done;
    Reg.Sta.(write empty)

  let init () =
    Printf.printf "Init\n%!";
    Gpio.(set_pull_state P18 PULL_DOWN);
    Gpio.(set_func P18 F_ALT5);

    Printf.printf "GPIO ready\n%!";
    stop ();
    Printf.printf "clock ready \n%!";
    Clock.kill ();
    Printf.printf "clock ready \n%!";
    Clock.set_pwm_clock (3 * 800000);
    Printf.printf "clock ready \n%!";

    
    Mem.set_int Reg.rng1 32;
    Reg.Ctl.(empty |> set clrf true |> write);
    Reg.Ctl.(empty |> set msen1 true |>set usef1 true |> set mode1 true |> write);
    Mtime.sleep_us 10L;
    Reg.Ctl.(empty |> set msen1 true |>set usef1 true |> set mode1 true |> set pwen1 true |> write);
    Printf.printf "PWM\n%!"


  let sta = Mem.(B.base + 0x04n)

  let rng1 = Mem.(B.base + 0x10n)
    
  let fif1 = Mem.(B.base + 0x18n)

  let write int_val = 
    while Reg.Sta.(read full1) do
      ()
    done;
    Mem.set_int fif1 int_val

end

