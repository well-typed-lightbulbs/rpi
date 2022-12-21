open Rpi.Mailbox

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
