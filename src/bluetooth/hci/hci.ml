(* HCI is the first layer for the bluetooth stack *)

module Packet = struct
  module Command = Command
  module ACL = Acl
  module Event = Event
end

type 'a expect =
  | Event of 'a Packet.Event.handler
  | Acl : 'a Acl.handler -> 'a Acl.t expect

type any = Event of Event.t | Acl of Cstruct.t Acl.t

exception Unexpected of int

open Lwt.Syntax

let wait ~get_byte =
  let* id = get_byte () in
  if id == Packet.Event.id then
    let+ evt = Packet.Event.read ~get_byte in
    Event evt
  else if id == Packet.ACL.id then
    let+ acl = Acl.read ~get_byte Acl.raw in
    Acl acl
  else raise (Unexpected id)

let expect ~get_byte (type a) (v : a expect) : a Lwt.t =
  let* id = get_byte () in
  match v with
  | Event e when id == Packet.Event.id -> Packet.Event.expect ~get_byte e
  | Acl a when id == Packet.ACL.id -> (Packet.ACL.read ~get_byte a : a Lwt.t)
  | _ -> raise (Unexpected id)

type 'a write =
  | Acl : 'a Acl.handler -> 'a Acl.t write
  | Command of 'a Command.command

let write ~write_byte (type a) (w : a write) (v : a) : unit =
  let buffer =
    match w with
    | Command cmd ->
        let cmd_size = Command.size cmd v in
        let buffer = Cstruct.create (1 + cmd_size) in
        Cstruct.set_uint8 buffer 0 Command.id;
        Command.write cmd v (Cstruct.sub buffer 1 cmd_size);
        buffer
    | Acl h ->
        let cmd_size = Acl.size h v in
        let buffer = Cstruct.create (1 + cmd_size) in
        Cstruct.set_uint8 buffer 0 Acl.id;
        Acl.write h v (Cstruct.sub buffer 1 cmd_size);
        buffer
  in
  for i = 0 to Cstruct.length buffer - 1 do
    let byte = Cstruct.get_uint8 buffer i in
    write_byte byte
  done

module LL = struct
  let scan_active = 0x01

  (* Connectable and scannable undirected advertising*)
  let adv_ind = 0x00

  (* Non connectable undirected advertising (ADV_NONCONN_IND *)
  let adv_nonconn_ind = 0x03
end

module Advert_data = Advert_data