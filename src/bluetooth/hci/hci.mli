module Packet : sig
  module Command = Command
  module ACL = Acl
  module Event = Event
end

type 'a expect =
  | Event of 'a Event.handler
  | Acl : 'a Acl.handler -> 'a Acl.t expect

type any = Event of Event.t | Acl of Cstruct.t Acl.t

exception Unexpected of int

val wait : get_byte:(unit -> int Lwt.t) -> any Lwt.t
val expect : get_byte:(unit -> int Lwt.t) -> 'a expect -> 'a Lwt.t

type 'a write =
  | Acl : 'a Acl.handler -> 'a Acl.t write
  | Command of 'a Command.t

val write : write_byte:(int -> unit) -> 'a write -> 'a -> unit

module LL : sig
  val scan_active : int
  val adv_ind : int
  val adv_nonconn_ind : int
end

module Advert_data = Advert_data
