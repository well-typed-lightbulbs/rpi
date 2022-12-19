(** Memory.

    {b Note.} All multi-bytes memory accesses are done in little endian
    order. *)

(** {1:addr Addresses} *)

type addr = nativeint
(** The type for byte addresses. *)

val ( + ) : addr -> addr -> addr
(** [a + off] adds [off] to [a]. *)

val ( - ) : addr -> addr -> addr
(** [a - off] subracts [off] to [a]. *)

val offset : addr -> int -> addr
(** [offset addr n] is [add + Nativeint.of_int n]. *)

val of_int32 : int32 -> addr
(** [of_int32 i] is the address corresponding to [i]. *)

val pp_addr : Format.formatter -> addr -> unit
(** [pp_addr ppf a] prints and unspecified reprsentation of [a]
      on [ppf]. *)

(** {1:barriers Memory barriers} *)

val wait : int -> unit
(** [wait n] waits at least [n] CPU cycles. *)

val dsb : unit -> unit
(** [dsb ()] performs a data synchronization barrier. Returns when
      all instructions before the call are complete. *)

val dmb : unit -> unit
(** [dmb ()] performs a data memory barrier. Ensures that all explicit
      memory access before the call complete before any new explicit
      access made after the call. *)

val isb : unit -> unit
(** [isb ()] performs an instruction synchronization barrier. Flushes
      the pipeline in the processor so that all instruction following the
      call are fetched from cache or memory. *)

(** {1:reads Reads} *)

val get_int : addr -> int
(** [get_int a] gets the 4 bytes starting at address [a].  *)

(** {1:writes Writes} *)

val set_int : addr -> int -> unit
(** [set a v] sets the 4-lowest bytes starting at address [a] to [v]. *)

val set_int_bits : addr -> bits:int -> int -> unit
(** [masked_set_int] is like {!set_int} but only affects the bits set
      in [bits]. *)

(** Memory maps *)
module Map : sig
  (** {1 Maps} *)

  type ('a, 'b) t = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
  (** The type for memory maps. *)

  type bytes = (int, Bigarray.int8_unsigned_elt) t
  (** The type for byte memory maps. *)

  type int32s = (int32, Bigarray.int32_elt) t
  (** The type for int32 memory maps. *)

  type int64s = (int64, Bigarray.int64_elt) t
  (** The type for int64 memory maps. *)

  val length : ('a, 'b) t -> int
  (** [length m] is [m]'s scalar length. *)

  val byte_length : ('a, 'b) t -> int
  (** [byte_length m] is [m]'s byte length. *)

  val base : ('a, 'b) t -> addr
  (** [base m] is [m]'s base address. *)

  val bytes : addr -> len:int -> bytes
  (** [bytes a len] maps [len] bytes starting at [a]. *)

  val int32s : addr -> len:int -> int32s
  (** [int32s a len] maps [len] int32 values starting at [a]. *)

  val int64s : addr -> len:int -> int64s
  (** [map_int64 a len] maps [len] int64 values starting at [a]. *)
end
