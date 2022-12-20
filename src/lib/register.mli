type 'a field = {
  offset : int;
  size : int;
  to_int : 'a -> int;
  of_int : int -> 'a;
}

val int : size:int -> offset:int -> int field
val bool : offset:int -> bool field
val raw : Optint.t field

module type Raw_S = sig
  type value

  val empty : value
  val get : value -> 'a field -> 'a
  val ( && ) : value -> 'a field -> 'a
  val set : 'a field -> 'a -> value -> value
end

module Raw : Raw_S

module type S = sig
  include Raw_S

  val read : unit -> value
  val write : value -> unit
end

module Make : functor
  (B : sig
     val addr : Optint.t
   end)
  -> S
