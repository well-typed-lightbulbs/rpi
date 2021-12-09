type t = { channel : int; data : Cstruct.t }

let read ~get_byte =
  let length =
    let length_lo = get_byte () in
    let length_hi = get_byte () in
    length_lo lor (length_hi lsl 8)
  in
  let channel =
    let handle_lo = get_byte () in
    let handle_hi = get_byte () in
    handle_lo lor (handle_hi lsl 8)
  in
  let data = Cstruct.create_unsafe length in
  for i = 0 to length - 1 do
    Cstruct.set_uint8 data i (get_byte ())
  done;
  { channel; data }

let size { data; _ } = Cstruct.length data + 4

let write { data; channel } buffer =
  let size = Cstruct.length data in
  Cstruct.LE.set_uint16 buffer 0 size;
  Cstruct.LE.set_uint16 buffer 2 channel;
  Cstruct.blit data 0 buffer 4 size
