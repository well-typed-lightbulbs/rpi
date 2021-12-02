open Mirage

type rpi4_mtime = Rpi4_mtime

let rpi4_mtime = Type.v Rpi4_mtime

type rpi4_clock = Rpi4_clock

let rpi4_clock = Type.v Rpi4_clock

type rpi4_gpio = Rpi4_gpio

let rpi4_gpio = Type.v Rpi4_gpio

type rpi4_pwm = Rpi4_pwm

let rpi4_pwm = Type.v Rpi4_pwm

let pin =
  "git+https://github.com/dinosaure/rpi.git#f56b44758220fb1447f1911b2d1630fee3b14f33"

let main = main "Unikernel.Make" (console @-> rpi4_mtime @-> rpi4_pwm @-> job)

let rpi4_mtime_impl ~unix =
  let packages =
    match unix with
    | true -> [ package ~pin "rpi" ~sublibs:[ "unix" ] ]
    | false -> [ package ~pin "rpi" ]
  in
  let connect _ _ _ = {ocaml|Lwt.return_unit|ocaml} in
  let module_name =
    match unix with
    | true -> "Rpi.Mtime.Make (Rpi_unix.Make (Rpi.Mtime))"
    | false -> "Rpi.Mtime.Make (Rpi.Mtime)"
  in
  impl ~packages ~connect module_name rpi4_mtime

let rpi4_mtime_impl =
  match_impl (Key.value Key.target)
    [
      (`RPi4, rpi4_mtime_impl ~unix:false); (`Unix, rpi4_mtime_impl ~unix:true);
    ]
    ~default:(rpi4_mtime_impl ~unix:true)

let rpi4_clock_impl ~unix =
  let packages =
    match unix with
    | true -> [ package ~pin "rpi" ~sublibs:[ "unix" ] ]
    | false -> [ package ~pin "rpi" ]
  in
  let connect _ _ _ = {ocaml|Lwt.return_unit|ocaml} in
  let module_name =
    match unix with
    | true -> "Rpi.Clock.Make (Rpi_unix.Make (Rpi.Clock))"
    | false -> "Rpi.Clock.Make (Rpi.Clock)"
  in
  impl ~packages ~connect module_name (rpi4_mtime @-> rpi4_clock)

let rpi4_clock_impl =
  match_impl (Key.value Key.target)
    [
      (`RPi4, rpi4_clock_impl ~unix:false); (`Unix, rpi4_clock_impl ~unix:true);
    ]
    ~default:(rpi4_clock_impl ~unix:true)

let rpi4_gpio_impl ~unix =
  let packages =
    match unix with
    | true -> [ package ~pin "rpi" ~sublibs:[ "unix" ] ]
    | false -> [ package ~pin "rpi" ]
  in
  let connect _ _ _ = {ocaml|Lwt.return_unit|ocaml} in
  let module_name =
    match unix with
    | true -> "Rpi.Gpio.Make (Rpi_unix.Make (Rpi.Gpio))"
    | false -> "Rpi.Gpio.Make (Rpi.Gpio)"
  in
  impl ~packages ~connect module_name rpi4_gpio

let rpi4_gpio_impl =
  match_impl (Key.value Key.target)
    [ (`RPi4, rpi4_gpio_impl ~unix:false); (`Unix, rpi4_gpio_impl ~unix:true) ]
    ~default:(rpi4_gpio_impl ~unix:true)

let rpi4_pwm_impl ~unix =
  let packages =
    match unix with
    | true -> [ package ~pin "rpi" ~sublibs:[ "unix" ] ]
    | false -> [ package ~pin "rpi" ]
  in
  let connect _ _ _ = {ocaml|Lwt.return_unit|ocaml} in
  let module_name =
    match unix with
    | true -> "Rpi.Pwm.Make (Rpi_unix.Make (Rpi.Pwm))"
    | false -> "Rpi.Pwm.Make (Rpi.Pwm)"
  in
  impl ~packages ~connect module_name
    (rpi4_gpio @-> rpi4_clock @-> rpi4_mtime @-> rpi4_pwm)

let rpi4_pwm_impl =
  match_impl (Key.value Key.target)
    [ (`RPi4, rpi4_pwm_impl ~unix:false); (`Unix, rpi4_pwm_impl ~unix:true) ]
    ~default:(rpi4_pwm_impl ~unix:true)

let rpi4_clock = rpi4_clock_impl $ rpi4_mtime_impl

let rpi4_pwm = rpi4_pwm_impl $ rpi4_gpio_impl $ rpi4_clock $ rpi4_mtime_impl

let () =
  register ~reporter:no_reporter ~argv:no_argv ~packages:[ package "rresult" ]
    "guirlande"
    [ main $ default_console $ rpi4_mtime_impl $ rpi4_pwm ]
