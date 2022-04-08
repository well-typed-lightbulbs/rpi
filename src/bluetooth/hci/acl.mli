val id : int

type 'a handler = {
  read : Cstruct.t -> 'a;
  write : 'a -> Cstruct.t -> unit;
  size : 'a -> int;
}

val raw : Cstruct.t handler

type 'a t = { handle : int; data : 'a }

val read : get_byte:(unit -> int Lwt.t) -> 'a handler -> 'a t Lwt.t
val size : 'a handler -> 'a t -> int
val write : 'a handler -> 'a t -> Cstruct.t -> unit
