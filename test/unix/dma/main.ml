open Rpi

type file_descr

external vcio_open : unit -> file_descr = "caml_vcio_open"
external vcio_write : file_descr -> Cstruct.t -> unit = "caml_vcio_write"
external mmap : nativeint -> nativeint -> nativeint = "caml_mmap"

external mmap2 :
  ('a, 'b) Stdlib.Bigarray.kind ->
  'c Stdlib.Bigarray.layout ->
  nativeint ->
  nativeint ->
  ('a, 'b, 'c) Stdlib.Bigarray.Genarray.t = "caml_ba_mmap"

let fd = vcio_open ()

module X = Mailbox.Make (struct
  let request = vcio_write fd
end)

let mmap_cstruct addr size =
  let buf =
    mmap2 Bigarray.char Bigarray.c_layout addr size
    |> Bigarray.array1_of_genarray
  in
  Cstruct.of_bigarray buf

module Pwm = Pwm.Make (struct
  open Pwm

  let mode = Serial
  let pins = [ Pin18 ]
  let freq = 3 * 800_000
  let range = 32
  let is_stereo = false
end)

module DD = Rpi.DMA.Make (struct
  let num = 5
end)

let () =
  let size = 1024 * 1024 in
  Printf.printf "Init\n%!";
  DD.Reg.Cs.(empty |> set reset true |> write);
  Mtime.sleep_us_sync 10L;
  DD.Reg.Cs.(empty |> write);
  Mem.dmb ();
  Mtime.sleep_us_sync 10L;
  Lwt_main.run (Pwm.stop ());
  Mem.dmb ();
  Mtime.sleep_us_sync 10L;
  Clock.kill ();
  Mem.dmb ();
  Mtime.sleep_us_sync 10L;

  let mem_ref = X.mem_alloc ~size ~align:256 ~flags:0xc in
  let bus_addr = X.mem_lock mem_ref in

  let buf =
    mmap_cstruct (Rpi_devices.bus_to_phys bus_addr) (Nativeint.of_int size)
  in
  Printf.printf "OK %nx\n%!" bus_addr;

  Lwt_main.run (Pwm.init ()) |> Result.get_ok;

  Printf.printf "PWM\n%!";

  let ti =
    Rpi.DMA.Transfer_information.(
      empty
      |> set source_address_increment true
      |> set no_wide_bursts true
      |> set wait_write_response true
      |> set destination_dreq true |> set peripheral_mapping Pwm
      (* |> set destination_address_increment true *)
      |> reg)
  in
  Rpi.DMA.Control_block.set_raw_transfer_information buf (Int32.of_int ti);
  Rpi.DMA.Control_block.set_raw_source_address buf
    (Nativeint.add bus_addr 256n |> Nativeint.to_int32);
  Rpi.DMA.Control_block.set_raw_destination_address buf
    (Mem.((* Rpi_devices.pwm0 *) 0x7e20c000n + 0x18n) |> Nativeint.to_int32);
  (* FIF1 *)
  Rpi.DMA.Control_block.set_raw_transfer_length buf (size |> Int32.of_int);
  Rpi.DMA.Control_block.set_raw_stride buf 0l;
  Rpi.DMA.Control_block.set_raw_next_control_address buf 0l;

  for i = 0 to size - 256 - 1 do
    Cstruct.set_uint8 buf (256 + i) (i mod 256)
  done;

  DD.Reg.Cs.(empty |> set reset true |> write);
  Mtime.sleep_us_sync 10L;
  DD.Reg.Cs.(empty |> set end' true |> set int true |> write);
  Mtime.sleep_us_sync 10L;

  DD.Reg.Conblk_ad.(empty |> set addr (bus_addr |> Nativeint.to_int) |> write);
  DD.Reg.Cs.(
    empty |> set active true |> set wait_for_outstanding_writes true |> write);

  Printf.printf "dma start\n%!";
  while
    (not (DD.Reg.Cs.(read end') || DD.Reg.Cs.(read error)))
    && DD.Reg.Cs.(read active)
  do
    Printf.printf ".%08x\n%!" (Mem.get_int DD.Reg.Debug.addr);
    Pwm.status ();

    Unix.sleepf 0.1
  done;
  if DD.Reg.Cs.(read end') then Printf.printf "\nyes\n%!"
  else Printf.printf "\nno\n%!";
  Lwt_main.run (Pwm.stop ());

  X.mem_unlock mem_ref;
  X.mem_free mem_ref
