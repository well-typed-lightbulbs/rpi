module type Base = sig
  val base : [< Version.t ] -> Mem.addr

  val registers_size : nativeint
end

module type S = sig
  type t

  val base : [< Version.t ] -> t

  val map : (size:nativeint -> Mem.addr -> Mem.addr) -> t -> t
end

module Make (Base : Base) = struct
  include Base

  type t = nativeint

  let map fn v = fn ~size:Base.registers_size v
end
