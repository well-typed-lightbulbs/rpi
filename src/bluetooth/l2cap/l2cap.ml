type t = { channel : int; data : Cstruct.t }

let read buffer =
  let length = Cstruct.LE.get_uint16 buffer 0 in
  let channel = Cstruct.LE.get_uint16 buffer 2 in
  let data = Cstruct.sub buffer 4 length in
  { channel; data }

let size { data; _ } = Cstruct.length data + 4

let write { data; channel } buffer =
  let size = Cstruct.length data in
  Cstruct.LE.set_uint16 buffer 0 size;
  Cstruct.LE.set_uint16 buffer 2 channel;
  Cstruct.blit data 0 buffer 4 size
