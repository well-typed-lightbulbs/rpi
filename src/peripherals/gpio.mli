include Peripheral.S

module type S = sig

  (** GPIO pins.

      {b References}
      {ul
      {- {{:https://www.raspberrypi.org/documentation/hardware/raspberrypi/gpio/README.md}GPIO Raspberry Pi documentation}.}
      {- Section 6 of the {{:https://www.raspberrypi.org/documentation/hardware/raspberrypi/bcm2835/BCM2835-ARM-Peripherals.pdf}BCM2835 ARM Peripheral specification} (PDF).}} *)

  (** {1:pins Pins} *)

  type pin = P00 | P01 | P02 | P03 | P04 | P05 | P06 | P07 | P08 | P09 | P10 
    | P11 | P12 | P13 | P14 | P15 | P16 | P17 | P18 | P19 | P20 | P21 | P22 | P23
    | P24 | P25 | P26 | P27 | P28 | P29 | P30 | P31 | P32 | P33 | P34 | P35 | P36
    | P37 | P38 | P39 | P40 | P41 | P42 | P43 | P44 | P45 | P46 | P47 | P48 | P49
    | P50 | P51 | P52 | P53
  [@@ocamlformat "disable"]
  (** The type for GPIO pins. *)

  (** {1:setup Pin setup} *)

  (** The type for pin functions. *)
  type func = F_IN | F_OUT | F_ALT5 | F_ALT4 | F_ALT0 | F_ALT1 | F_ALT2 | F_ALT3

  val set_func :pin -> func -> unit
  (** [set p func] sets the function of pin [p] to [func]. *)

  (** The type for pin pull state. *)
  type pull_state = PULL_OFF | PULL_DOWN | PULL_UP

  val set_pull_state :pin -> pull_state -> unit
  (** [set p state] sets the pull state of pin [p] to [state]. *)

  (** {1:rw Read and write} *)

  val set :pin -> bool -> unit
  (** [set p v] sets the value of pin [p] to [v]. *)

end

module Make(_: Peripheral.Base) : S
