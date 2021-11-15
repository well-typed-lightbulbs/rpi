include Peripheral.S

val init : clock:Clock.t -> gpio:Gpio.t -> t -> unit

val write : t -> int -> unit
