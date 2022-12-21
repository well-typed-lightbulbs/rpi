module UART0 : sig
  val read_byte : unit -> int Lwt.t
  val write_byte : int -> unit
  val init : unit -> unit
end
