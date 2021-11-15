let strf = Format.asprintf

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

let channel_to_int32 : channel -> int32 =
 fun m -> Int32.of_int (Obj.magic (Obj.repr m) : int)

let mbox_base = Mem.(Mmio.base + 0xB880n)

let mbox_read = Mem.(mbox_base + 0x00n)

let mbox_peek = Mem.(mbox_base + 0x10n)

let mbox_sender = Mem.(mbox_base + 0x14n)

let mbox_status = Mem.(mbox_base + 0x18n)

let mbox_config = Mem.(mbox_base + 0x1Cn)

let mbox_write = Mem.(mbox_base + 0x20n)

let empty = 0x40000000l

let full = 0x80000000l

let msg channel v = Int32.(logor v channel)

let msg_channel r = Int32.logand r 0xFl

let msg_value r = Nativeint.of_int32 Int32.(logand r 0xFFFF_FFF0l)

let block_on s =
  while Int32.logand (Mem.get_int32 mbox_status) s <> 0l do
    ()
  done

let read c =
  let c = channel_to_int32 c in
  let rec loop () =
    block_on empty;
    Mem.dmb ();
    let m = Mem.get_int32 mbox_read in
    Mem.dmb ();
    if Int32.compare (msg_channel m) c = 0 then msg_value m else loop ()
  in
  loop ()

let write c v =
  let c = channel_to_int32 c in
  let v = Nativeint.to_int32 v in
  let loop () =
    block_on full;
    Mem.set_int32 mbox_write (msg c v)
  in
  loop ()

(* Property interface *)

module Prop = struct
  (* Property values *)

  type 'a t =
    | Unit : unit t
    | Bytes : int * (Mem.Map.bytes -> ('a, [ `Msg of string ]) result) -> 'a t
    | Int32 : int * (Mem.Map.int32s -> ('a, [ `Msg of string ]) result) -> 'a t
    | Int64 : int * (Mem.Map.int64s -> ('a, [ `Msg of string ]) result) -> 'a t

  let prop_byte_length (type a) (p : a t) =
    match p with
    | Unit -> 0
    | Bytes (count, _) -> count
    | Int32 (count, _) -> 4 * count
    | Int64 (count, _) -> 8 * count

  let err_short len exp =
    Error (`Msg (strf "response value too short exp:%d got:%d" exp len))

  let id x = x

  let get1 conv m =
    let len = Mem.Map.length m in
    if len < 1 then err_short len 1 else Ok (conv m.{0})

  let get2 conv m =
    let len = Mem.Map.length m in
    if len < 2 then err_short len 2 else Ok (conv m.{0}, conv m.{1})

  let unit = Unit

  let int = Int32 (1, get1 Int32.to_int)

  let int32 = Int32 (1, get1 id)

  let int64 = Int64 (1, get1 id)

  let int_pair = Int32 (2, get2 Int32.to_int)

  let int32_pair = Int32 (2, get2 id)

  let int64_pair = Int64 (2, get2 id)

  let string ~max =
    let parse m =
      let len = Mem.Map.length m in
      let b = Bytes.create len in
      let max = len - 1 in
      for i = 0 to max do
        Bytes.unsafe_set b i (Char.unsafe_chr m.{i})
      done;
      Ok (Bytes.unsafe_to_string b)
    in
    Bytes (max, parse)

  (* Requests *)

  type args = int32 list

  type ereq = { tag : int32; args : args; buf_blen : int }

  type 'a req = ereq * 'a t

  let req ?(args = []) tag ~resp:prop =
    let buf_blen = max (4 * List.length args) (prop_byte_length prop) in
    let buf_blen =
      let rem = buf_blen mod 4 in
      if rem = 0 then buf_blen else buf_blen + (4 - rem)
    in
    ({ tag; args; buf_blen }, prop)

  let req_byte_length r =
    4 (* tag *) + 4 (* val len *) + 4 (* indicator *) + r.buf_blen

  let r (ereq, _) = ereq

  (* Responses *)

  type resp = {
    msg : Mem.Map.int32s;
    (* Tag, (start of value in msg, byte length) *)
    tag_index : (Int32.t * (int * int)) list;
  }

  let msg_addr = 0x1000n
  (* The location is free but FIXME. This should be allocated via
     malloc on a 16 byte boundary. A function should be added for
     that in Mem. It must be possible to do that by simply
     allocating a bigarray with Bigarray.Array1.create with + 16
     bytes an extracting a sub map out of it.

     FIXME Besides should this address be sent as a VC CPU Bus
     address ? Unclear. *)

  let request_msg reqs =
    let add_req_blen acc r = acc + req_byte_length r in
    let reqs_blen = List.fold_left add_req_blen 0 reqs in
    let msg_blen = 4 (* size *) + 4 (* code *) + reqs_blen + 4 (* end *) in
    let m = Mem.Map.int32s msg_addr ~len:(msg_blen / 4) in
    let rec add_reqs i = function
      | [] -> m.{i} <- 0l (* end tag *)
      | r :: reqs ->
          let next = i + 2 + (r.buf_blen / 4) + 1 in
          m.{i} <- r.tag;
          m.{i + 1} <- Int32.of_int r.buf_blen;
          m.{i + 2} <- Int32.of_int @@ (4 * List.length r.args);
          ignore
            (List.fold_left
               (fun i a ->
                 m.{i} <- a;
                 i + 1)
               (i + 3) r.args);
          add_reqs next reqs
    in
    m.{0} <- Int32.of_int msg_blen;
    m.{1} <- 0l;
    (* Request *)
    add_reqs 2 reqs;
    m

  let err_unterminated = `Msg "unterminated response"

  let err_truncated_resp t = `Msg (strf "truncated response (tag:%lX)" t)

  let err_unknown_code c = `Msg (strf "unknown reponse code: %lX" c)

  let err_parse = `Msg (strf "GPU: error parsing request")

  let err_addr a a' =
    `Msg (strf "different return address %a %a" Mem.pp_addr a Mem.pp_addr a')

  let tag_index m =
    let max = Mem.Map.length m - 1 in
    let rec loop acc i =
      if i > max then Error err_unterminated
      else
        let tag = m.{i} in
        if tag = 0l then Ok acc
        else if i + 2 > max then Error (err_truncated_resp tag)
        else
          let buf_blen = m.{i + 1} (* n.b. this is multiple of 4. *) in
          let next = i + 2 + (Int32.to_int buf_blen / 4) + 1 in
          let indic = m.{i + 2} in
          let acc =
            if Int32.logand 0x8000_0000l indic = 0l then acc
            else
              (tag, (i + 3, Int32.(to_int (logand 0x7FFF_FFFFl indic)))) :: acc
          in
          loop acc next
    in
    loop [] 2

  let send reqs =
    let msg = request_msg reqs in
    let addr = Mem.Map.base msg in
    write Tags_ARM_to_VC addr;
    let addr' = read Tags_ARM_to_VC in
    if addr <> addr' then Error (err_addr addr addr')
    else
      match msg.{1} with
      | 0x80000000l -> (
          match tag_index msg with
          | Ok tag_index -> Ok { msg; tag_index }
          | Error _ as e -> e)
      | 0x80000001l -> Error err_parse
      | c -> Error (err_unknown_code c)

  let find (type a) resp (ereq, (prop : a t)) : (a option, _) result =
    try
      let some = function Ok v -> Ok (Some v) | Error _ as e -> e in
      let pos, len = List.assoc ereq.tag resp.tag_index in
      match prop with
      | Unit -> Ok (Some ())
      | Bytes (_, parse) ->
          let addr = Mem.(offset (Map.base resp.msg) @@ (4 * pos)) in
          some @@ parse (Mem.Map.bytes addr ~len)
      | Int32 (_, parse) ->
          some @@ parse (Bigarray.Array1.sub resp.msg pos (len / 4))
      | Int64 (_, parse) ->
          let addr = Mem.(offset (Map.base resp.msg) @@ (4 * pos)) in
          some @@ parse (Mem.Map.int64s addr ~len:(len / 8))
    with Not_found -> Ok None
end
