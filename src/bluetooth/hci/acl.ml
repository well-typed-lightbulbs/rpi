let id = 0x02

type 'a handler = {
  read : Cstruct.t -> 'a;
  write : 'a -> Cstruct.t -> unit;
  size : 'a -> int;
}

let raw =
  {
    read = Fun.id;
    write = (fun src dst -> Cstruct.blit src 0 dst 0 (Cstruct.length src));
    size = Cstruct.length;
  }

type 'a t = { handle : int; data : 'a }

let read ~get_byte v =
  let open Lwt.Syntax in
  let* handle =
    let* handle_lo = get_byte () in
    let+ handle_hi = get_byte () in
    handle_lo lor (handle_hi lsl 8)
  in
  let* length =
    let* length_lo = get_byte () in
    let+ length_hi = get_byte () in
    length_lo lor (length_hi lsl 8)
  in
  let buffer = Cstruct.create_unsafe length in
  let+ () = 
    List.init length Fun.id |>
    Lwt_list.iter_s (fun i -> 
      let+ b = get_byte () in
      Cstruct.set_uint8 buffer i b)
  in
  { handle; data = v.read buffer }

let size { size; _ } { data; _ } = size data + 4

let write { write; size; _ } { data; handle } buffer =
  let size = size data in
  Cstruct.LE.set_uint16 buffer 0 handle;
  Cstruct.LE.set_uint16 buffer 2 size;
  write data (Cstruct.sub buffer 4 size)
