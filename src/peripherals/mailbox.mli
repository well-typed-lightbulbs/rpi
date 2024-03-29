type handle

val mem_alloc : size:int -> align:int -> flags:int -> handle
val mem_free : handle -> unit
val mem_lock : handle -> Mem.addr
val mem_unlock : handle -> unit
