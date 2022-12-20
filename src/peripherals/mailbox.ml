type 'a request = {
  tag : int;
  size : int;
  parameters : Cstruct.t -> 'a -> unit;
}

let mem_alloc =
  {
    tag = 0x3000c;
    size = 12;
    parameters =
      (fun buf (size, align, flags) ->
        let open Cstruct in
        LE.set_uint32 buf 0 size;
        LE.set_uint32 buf 4 align;
        LE.set_uint32 buf 8 flags);
  }

let handle buf handle = Cstruct.LE.set_uint32 buf 0 handle
let mem_free = { tag = 0x3000f; size = 4; parameters = handle }
let mem_lock = { tag = 0x3000d; size = 4; parameters = handle }
let mem_unlock = { tag = 0x3000e; size = 4; parameters = handle }

let serialize { tag; size; parameters } v =
  let total_size = 4 + 4 + 4 + 4 + 4 + size + 4 in
  let open Cstruct in
  let b = create_unsafe total_size in
  LE.set_uint32 b 0 (Int32.of_int total_size);
  LE.set_uint32 b 4 0l;
  LE.set_uint32 b 8 (Int32.of_int tag);
  LE.set_uint32 b 12 (Int32.of_int size);
  LE.set_uint32 b 16 (Int32.of_int size);
  parameters (Cstruct.sub b 20 size) v;
  LE.set_uint32 b (20 + size) 0l;
  b

type handle = int32

let request cst = Rpi_hardware.mbox_request cst.Cstruct.buffer

let mem_alloc ~size ~align ~flags =
  let buf =
    serialize mem_alloc
      (Int32.of_int size, Int32.of_int align, Int32.of_int flags)
  in
  request buf;
  Cstruct.LE.get_uint32 buf 20

let mem_free h = request (serialize mem_free h)

let mem_lock h =
  let buf = serialize mem_lock h in
  request buf;
  Cstruct.LE.get_uint32 buf 20 |> Optint.of_unsigned_int32

let mem_unlock h = request (serialize mem_unlock h)

let with_buffer ~size ~align ~flags fn =
  Printf.printf "+ %d\n%!" size;
  let mem_ref = mem_alloc ~size ~align ~flags in
  let bus_addr = mem_lock mem_ref in
  let finally () =
    Printf.printf "- %d\n%!" size;
    mem_unlock mem_ref;
    mem_free mem_ref
  in
  Fun.protect (fun () -> fn bus_addr) ~finally

let with_buffer_lwt ~size ~align ~flags fn =
  Printf.printf "+ %d\n%!" size;
  let mem_ref = mem_alloc ~size ~align ~flags in
  let bus_addr = mem_lock mem_ref in
  let finally () =
    Printf.printf "- %d\n%!" size;
    mem_unlock mem_ref;
    mem_free mem_ref;
    Lwt.return_unit
  in
  Lwt.finalize (fun () -> fn bus_addr) finally
