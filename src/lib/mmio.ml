(** Memory mapped IO. *)
let base = 0xfe000000n
(** The base ARM physical address at which memory mapped IO is available.
      On a RPiv2 this is [0x3F000000]. On previous models (unsupported
      for now) it was [0x20000000]. *)
