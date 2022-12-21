module type S = sig
  val read_byte : unit -> int Lwt.t
  val write_byte : int -> unit
end

module UART0 : sig
  include S

  val init : unit -> unit
  val restart_threads : unit -> unit
end