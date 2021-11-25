val mmap : size:nativeint -> Mem.addr -> Mem.addr

module Make (S : Peripheral.S) : S.Base
