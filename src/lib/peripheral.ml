module type Base = sig
  val base : Mem.addr

end

module type S = sig
  val base : Mem.addr

  val registers_size : nativeint
end
