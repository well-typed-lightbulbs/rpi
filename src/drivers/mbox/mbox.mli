val with_buffer :
  size:int -> align:int -> flags:int -> (Mem.addr -> 'a) -> 'a

val with_buffer_lwt :
  size:int -> align:int -> flags:int -> (Mem.addr -> 'a Lwt.t) -> 'a Lwt.t
