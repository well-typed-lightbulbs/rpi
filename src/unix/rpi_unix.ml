(* bring addr into userspace *)

external mmap : Mem.addr -> nativeint -> Mem.addr = "caml_mmap"

let mmap ~size v = mmap v size

module Make (P : Peripheral.S) = struct
  type t = P.t

  let base = mmap ~size:P.registers_size P.base
end
