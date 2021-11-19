include Peripheral.S

val init : mtime:Mtime.t -> clock:Clock.t -> gpio:Gpio.t -> t -> unit

val write : t -> int32 -> unit

val stop : t -> unit