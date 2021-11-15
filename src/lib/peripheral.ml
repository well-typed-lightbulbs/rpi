module type S = sig
  type t

  val base : [< Version.t ] -> t

  val map : (Mem.addr -> Mem.addr) -> t -> t
end

type t = Mem.addr

let map fn v = fn v
