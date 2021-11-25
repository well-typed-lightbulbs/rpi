include Peripheral.S

module type S = sig
  val init : unit -> unit

  val write : int -> unit

  val stop : unit -> unit
end

module Make (Gpio : Gpio.S) (Clock : Clock.S) (Mtime : Mtime.S) (_ : Base) : S
