(** Mailboxes.

    Mailbox allow to connect with the RPi's GPU which is in charge
    of a lot the bookkeeping.

    This is not very well documented, a bit of documentation can be found {{:https://github.com/raspberrypi/firmware/wiki/Mailboxes}here}.
    Using the {{!propi}property} interface should be enough. *)

(** {1:propi Property interface} *)

(** Property interface

      Convenience interface for the
      {{:https://github.com/raspberrypi/firmware/wiki/Mailbox-property-interface}mailbox property interface} (channel [Tags_ARM_to_VC]). *)
module Prop : sig
  (** {1 Property values} *)

  (** The type for response property values. The expected maximal
        length of the response and a property parser. The map given to
        the property parser can be shorter than the specified
        length. *)
  type 'a t =
    | Unit : unit t
    | Bytes : int * (Mem.Map.bytes -> ('a, [ `Msg of string ]) result) -> 'a t
    | Int32 : int * (Mem.Map.int32s -> ('a, [ `Msg of string ]) result) -> 'a t
    | Int64 : int * (Mem.Map.int64s -> ('a, [ `Msg of string ]) result) -> 'a t

  val unit : unit t
  (** [unit] is an empty property. *)

  val string : max:int -> string t
  (** [string] is a string of maximum length [max]. *)

  val int : int t
  (** [int] is an integer property (parsed from an int32). *)

  val int32 : int32 t
  (** [int32] is an int32 property. *)

  val int64 : int64 t
  (** [int64] is an int64 property. *)

  val int_pair : (int * int) t
  (** [int] is an integer pair property (parsed from two int32s). *)

  val int32_pair : (int32 * int32) t
  (** [int32_pair] is an int32 pair property. *)

  val int64_pair : (int64 * int64) t
  (** [int32_pair] is an int64 pair property. *)

  (** {1:req Requests} *)

  type args = int32 list
  (** The type for property requests arguments. FIXME this
        should maybe be generalized but at the moment all published
        requests arguments are uint32. *)

  type 'a req
  (** The type for requests of properties of type 'a. *)

  type ereq
  (** The type for existential requests. *)

  val req : ?args:args -> int32 -> resp:'a t -> 'a req
  (** [req t args resp] is a property for tag [t] with request arguments
        [args] (defaults to []) and response property parsed with [resp]. *)

  val r : 'a req -> ereq
  (** [r req] is an existential request for [r]. *)

  (** {1:resp Responses} *)

  type resp
  (** The type for responses. *)

  val send : ereq list -> (resp, [ `Msg of string ]) result
  (** [send reqs] sends the list of requests [reqs] in the given order. *)

  val find : resp -> 'a req -> ('a option, [ `Msg of string ]) result
  (** [find resp req] finds the property value of [req] in response [resp].
        [None] is returned if the property can't be found. *)
end

(** {1:channels Channels} *)

(** The type for mailbox channels *)
type channel =
  | Power_management
  | Framebuffer
  | Virtual_UART
  | VCHIQ
  | LEDs
  | Buttons
  | Touchscreen
  | Unused
  | Tags_ARM_to_VC
  | Tags_VC_to_ARM

(** {1:addr Communicating addresses}

      {b Warning} The lowest 4-bits of sent addresses can't be read
      by the GPU as they are used to transmit the channel.
      used. As such addresses must be aligned on 16 bytes. *)

val read : channel -> Mem.addr
(** [read c] reads the address of channel [c]. *)

val write : channel -> Mem.addr -> unit
(** [write c a] writes the [v] to the channel [c]. *)
