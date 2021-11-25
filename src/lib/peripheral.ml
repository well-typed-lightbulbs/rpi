module type S = sig
  type t

  val base : Mem.addr

  val registers_size : nativeint

  module type Base = sig
    type nonrec t = t

    val base : Mem.addr
  end
end

module Make (B : sig
  val base : Mem.addr

  val registers_size : nativeint
end) : S = struct
  type t

  let base = B.base

  let registers_size = B.registers_size

  module type Base = sig
    type nonrec t = t

    val base : Mem.addr
  end
end
