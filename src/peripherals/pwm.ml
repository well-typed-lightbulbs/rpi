type mode = Serial | Analog
type pin_nr = Pin12 | Pin13 | Pin18 | Pin19 | Pin40 | Pin41 | Pin45

let to_gpio = function
  | Pin12 -> Gpio.P12
  | Pin13 -> Gpio.P13
  | Pin18 -> Gpio.P18
  | Pin19 -> Gpio.P19
  | Pin40 -> Gpio.P40
  | Pin41 -> Gpio.P41
  | Pin45 -> Gpio.P45

let assigned_instance = function
  | Pin12 | Pin13 | Pin18 | Pin19 | Pin45 -> Rpi_hardware.pwm0
  | Pin40 | Pin41 -> Rpi_hardware.pwm1

let assigned_func = function
  | Pin12 | Pin13 | Pin40 | Pin41 | Pin45 -> Gpio.F_ALT0
  | Pin18 | Pin19 -> Gpio.F_ALT5

module Make (Setting : sig
  val mode : mode
  val pins : pin_nr list
  val freq : int
  val range : int
  val is_stereo : bool
end) =
struct
  let base =
    let inst =
      List.fold_left
        (fun acc pin ->
          match (acc, pin) with
          | None, pin -> Some (assigned_instance pin)
          | Some inst, pin ->
              if inst = assigned_instance pin then acc
              else failwith "Sure those pins are right?")
        None Setting.pins
    in
    match inst with
    | None -> failwith "Please, provide a pin"
    | Some inst -> inst

  module Reg = struct
    module Ctl = struct
      include Register.Make (struct
        let addr = base
      end)

      let msen1 = bool ~offset:7
      let clrf = bool ~offset:6
      let usef1 = bool ~offset:5
      let usef2 = bool ~offset:13
      let mode1 = bool ~offset:1
      let pwen1 = bool ~offset:0
      let pwen2 = bool ~offset:8
      let clrf = bool ~offset:6
    end

    module Sta = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x04n)
      end)

      let empt1 = bool ~offset:1
      let full1 = bool ~offset:0
      let gapo0 = bool ~offset:5
      let berr = bool ~offset:8
    end

    module Dmac = struct
      include Register.Make (struct
        let addr = Mem.(base + 0x08n)
      end)

      let enab = bool ~offset:31
      let dreq = int ~offset:0 ~size:8
      let panic = int ~offset:8 ~size:8
    end

    let rng1 = Mem.(base + 0x10n)
    let rng2 = Mem.(base + 0x20n)
    let fif1 = Mem.(base + 0x18n)
  end

  open Lwt.Syntax

  let write int_val =
    let rec loop () =
      if Reg.Sta.(read full1) then
        let* () = Lwt.pause () in
        loop ()
      else Lwt.return_unit
    in
    let+ () = loop () in
    Mem.set_int Reg.fif1 int_val

  let write_sync int_val =
    while Reg.Sta.(read full1) do
      ()
    done;
    Mem.set_int Reg.fif1 int_val

  let flush () =
    let rec loop () =
      if not Reg.Sta.(read empt1) then
        let* () = Lwt.pause () in
        loop ()
      else Lwt.return_unit
    in
    loop ()

  let stop () =
    Reg.Ctl.(empty |> set clrf true |> write);
    let+ () = flush () in
    Reg.Ctl.(write empty)

  let status () =
    Printf.printf "a: %08x\n%!" (Mem.get_int Reg.Sta.addr);
    if Reg.Sta.(read berr) then Reg.Sta.(empty |> set berr true |> write)

  let init () =
    Mem.dmb ();

    (* ? -> GPIO *)
    List.iter
      (fun pin ->
        let gpio = to_gpio pin in
        let func = assigned_func pin in
        Gpio.(set_pull_state gpio PULL_DOWN);
        Gpio.(set_func gpio func))
      Setting.pins;

    Mem.dmb ();

    (* PWM -> CLOCK *)
    let clock_result =
      match Clock.freq () with
      | Some v -> Error v
      | None ->
          Mtime.sleep_us_sync 10L;
          Clock.kill ();
          Mtime.sleep_us_sync 10L;
          Clock.set_pwm_clock Setting.freq
    in
    Mem.dmb ();

    (* CLOCK -> PWM *)
    let* () = stop () in
    let+ () = Mtime.sleep_us 2000L in
    Mem.set_int Reg.rng1 Setting.range;
    if Setting.is_stereo then Mem.set_int Reg.rng2 Setting.range;

    Mtime.sleep_us_sync 10L;

    Reg.Ctl.(empty |> set clrf true |> write);

    Mtime.sleep_us_sync 10L;

    Reg.Dmac.(empty |> set enab true |> set panic 7 |> set dreq 3 |> write);

    Mtime.sleep_us_sync 10L;

    let open Reg.Ctl in
    let ch2 =
      if Setting.is_stereo then
        Reg.Ctl.(empty |> set usef2 true |> set pwen2 true)
      else Reg.Ctl.empty
    in
    let ch1 prev = prev |> set usef1 true |> set pwen1 true in
    let set_mode prev =
      match Setting.mode with
      | Serial -> prev |> set mode1 true
      | Analog -> prev |> set mode1 false
    in
    ch2 |> ch1 |> set_mode |> write;
    clock_result
end
