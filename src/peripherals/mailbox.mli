type handle

val mem_alloc : size:int -> align:int -> flags:int -> handle
val mem_free : handle -> unit
val mem_lock : handle -> Mem.addr
val mem_unlock : handle -> unit

val with_buffer :
  size:int -> align:int -> flags:int -> (Mem.addr -> unit) -> unit

val with_buffer_lwt :
  size:int -> align:int -> flags:int -> (Mem.addr -> unit Lwt.t) -> unit Lwt.t
