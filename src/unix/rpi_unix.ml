(* bring addr into userspace *)

external mmap : Mem.addr -> nativeint -> Mem.addr = "caml_mmap"

let mmap ~size v = mmap v size