(* 32 bit registers *)
module Raw : sig
  type 'a field = {
    offset : int;
    size : int;
    to_int : 'a -> int;
    of_int : int -> 'a;
  }

  val int : size:int -> offset:int -> int field
  val bool : offset:int -> bool field
  val get : int -> 'a field -> 'a

  type value

  val empty : value
  val of_int : int -> value
  val set : 'a field -> 'a -> value -> value
  val reg : value -> int
end = struct
  type 'a field = {
    offset : int;
    size : int;
    to_int : 'a -> int;
    of_int : int -> 'a;
  }

  let get value { offset; size; of_int; _ } =
    of_int ((value lsr offset) land ((1 lsl size) - 1))

  type value = int

  let of_int = Fun.id
  let empty : value = 0

  let set { offset; size; to_int; _ } value (current : value) : value =
    let mask = (1 lsl size) - 1 in
    current land lnot (mask lsl offset) lor (to_int value lsl offset)

  let reg (v : value) = v

  let bool ~offset =
    {
      offset;
      size = 1;
      to_int = (fun x -> if x then 1 else 0);
      of_int = (fun x -> x == 1);
    }

  let int ~size ~offset = { offset; size; to_int = Fun.id; of_int = Fun.id }
end

module Make (B : sig
  val addr : Mem.addr
end) : sig
  include module type of Raw

  val read : 'a field -> 'a
  val write : value -> unit
  val addr : Mem.addr
end = struct
  include Raw

  let addr = B.addr
  let read t = get (Mem.get_int B.addr) t
  let write (v : value) = Mem.set_int B.addr (reg v)
end
