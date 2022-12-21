type 'a field = {
  offset : int;
  size : int;
  to_int : 'a -> int;
  of_int : int -> 'a;
}

let bool ~offset =
  {
    offset;
    size = 1;
    to_int = (fun x -> if x then 1 else 0);
    of_int = (fun x -> x == 1);
  }

let int ~size ~offset = { offset; size; to_int = Fun.id; of_int = Fun.id }

let raw =
  { offset = 0; size = 32; to_int = Optint.to_int; of_int = Optint.of_int }

module type Raw_S = sig
  type value

  val empty : value
  val get : value -> 'a field -> 'a
  val ( && ) : value -> 'a field -> 'a
  val set : 'a field -> 'a -> value -> value
end

(* 32 bit registers *)
module Raw : Raw_S with type value = Optint.t = struct
  open Optint.Infix

  let ( land ) = Optint.logand
  let ( lor ) = Optint.logor
  let ( lsl ) = Optint.shift_left
  let ( lsr ) = Optint.shift_right_logical
  let lnot = Optint.lognot
  let one = Optint.one

  type value = Optint.t

  let empty : value = Optint.zero

  let get value { offset; size; of_int; _ } =
    of_int (Optint.to_int ((value lsr offset) land ((one lsl size) - one)))

  let ( && ) = get

  let set { offset; size; to_int; _ } value (current : value) : value =
    let mask = (one lsl size) - one in
    current
    land lnot (mask lsl offset)
    lor ((to_int value |> Optint.of_int) lsl offset)
end

module type S = sig
  include Raw_S

  val read : unit -> value
  val write : value -> unit
end

module Make (B : sig
  val addr : Mem.addr
end) : S = struct
  include Raw

  (* TODO 32 bit registers on 32 bit vals*)
  let read () = Mem.get_int B.addr |> Optint.of_int
  let write (v : value) = Mem.set_int B.addr (Optint.to_int v)
end
