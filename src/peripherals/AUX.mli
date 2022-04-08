(* read one char on the miniUART line *)
val read : unit -> char Lwt.t

val restart_threads : unit -> unit

val init : unit -> unit
