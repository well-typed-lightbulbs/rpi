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

external get_int : addr -> int = "ocamlrpi_mem_get_int" [@@noalloc]

(* Writes *)

external set_int : addr -> int -> unit = "ocamlrpi_mem_set_int" [@@noalloc]

(* Masked writes *)

external set_int_bits : addr -> bits:int -> int -> unit
  = "ocamlrpi_mem_set_int_bits"
  [@@noalloc]
