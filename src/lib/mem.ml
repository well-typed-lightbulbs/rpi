(* Addresses and memory barriers *)

type addr = Optint.t

external wait : int -> unit = "ocamlrpi_barrier_wait" [@@noalloc]
external dsb : unit -> unit = "ocamlrpi_barrier_dsb" [@@noalloc]
external dmb : unit -> unit = "ocamlrpi_barrier_dmb" [@@noalloc]
external isb : unit -> unit = "ocamlrpi_barrier_isb" [@@noalloc]

let ( + ) = Optint.add
let ( - ) = Optint.sub
let offset a off = Optint.(add a (of_int off))
let of_int32 = Optint.of_int32
let pp_addr ppf a = Optint.pp ppf a

(* Reads *)

external get : addr -> int = "ocamlrpi_mem_get_byte" [@@noalloc]
external get_int : addr -> int = "ocamlrpi_mem_get_int" [@@noalloc]
external get_int32 : addr -> int32 = "ocamlrpi_mem_get_int32"
external get_int64 : addr -> int64 = "ocamlrpi_mem_get_int64"

(* Writes *)

external set : addr -> int -> unit = "ocamlrpi_mem_set_byte" [@@noalloc]
external set_int : addr -> int -> unit = "ocamlrpi_mem_set_int" [@@noalloc]

external set_int32 : addr -> int32 -> unit = "ocamlrpi_mem_set_int32"
  [@@noalloc]

external set_int64 : addr -> int64 -> unit = "ocamlrpi_mem_set_int64"
  [@@noalloc]

(* Masked writes *)

external set_bits : addr -> bits:int -> int -> unit
  = "ocamlrpi_mem_set_byte_bits"
  [@@noalloc]

external set_int_bits : addr -> bits:int -> int -> unit
  = "ocamlrpi_mem_set_int_bits"
  [@@noalloc]

external set_int32_bits : addr -> bits:int32 -> int32 -> unit
  = "ocamlrpi_mem_set_int32_bits"
  [@@noalloc]

external set_int64_bits : addr -> bits:int64 -> int64 -> unit
  = "ocamlrpi_mem_set_int64_bits"
  [@@noalloc]
