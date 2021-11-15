(** Serial connection.

    [Serial] gives access to a
    {{:http://elinux.org/RPi_Serial_Connection} serial connection}
    using the Pi's UART0 peripheral.

    {b References}
    {ul
    {- Section 13 of the {{:https://www.raspberrypi.org/documentation/hardware/raspberrypi/bcm2835/BCM2835-ARM-Peripherals.pdf}BCM2835 ARM Peripheral specification} (PDF).}} *)

(** {1:init Initialization} *)

val init : unit -> unit
(** [init ()] initializes the serial connection. *)

(** {1:read Read} *)

(* val read_byte : unit -> int
   (** [read_byte ()] blocks until a byte becomes available on the
       serial connection. *)

   val try_read_byte : unit -> int option
   (** [try_read_byte ()] is [Some b] a byte could be read from the
       serial connection and [None] otherwise. *) *)

(** {1:write Write} *)

val write_byte : int -> unit
(** [write_byte b] writes the byte [b] on the serial connection. *)

val write : string -> unit
(** [write s] writes [s] on the serial connection. *)

val writef : ('a, Format.formatter, unit) format -> 'a
(** [writef fmt ...] write a string formatted according to [fmt]
      on the serial connection. *)
