(* Addresses and memory barriers *)

type addr = nativeint

external wait : int -> unit = "ocamlrpi_barrier_wait" [@@noalloc]

external dsb : unit -> unit = "ocamlrpi_barrier_dsb" [@@noalloc]

external dmb : unit -> unit = "ocamlrpi_barrier_dmb" [@@noalloc]

external isb : unit -> unit = "ocamlrpi_barrier_isb" [@@noalloc]

let ( + ) = Nativeint.add

let ( - ) = Nativeint.sub

let offset a off = Nativeint.(add a (of_int off))

let of_int32 = Nativeint.of_int32

let pp_addr ppf a = Format.fprintf ppf "0x%nX" a

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

external set_int32_pow : addr -> int -> unit = "ocamlrpi_mem_set_int32_pow"
  [@@noalloc]

(* Mapping *)

module Map = struct
  type ('a, 'b) t = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t

  type bytes = (int, Bigarray.int8_unsigned_elt) t

  type int32s = (int32, Bigarray.int32_elt) t

  type int64s = (int64, Bigarray.int64_elt) t

  let length = Bigarray.Array1.dim

  external byte_length : ('a, 'b) t -> int = "ocamlrpi_mem_map_byte_length"

  external base : ('a, 'b) t -> addr = "ocamlrpi_mem_map_base"

  external bytes : addr -> len:int -> bytes = "ocamlrpi_mem_map_bytes"

  external int32s : addr -> len:int -> int32s = "ocamlrpi_mem_map_int32"

  external int64s : addr -> len:int -> int64s = "ocamlrpi_mem_map_int64"
end
