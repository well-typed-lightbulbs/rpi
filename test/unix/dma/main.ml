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

module PwmAudio = Pwm.Make (struct
  open Pwm

  let mode = Analog
  let pins = [ Pin40; Pin41 ]
  let freq = 54 * 1_000_000 / 2
  let range = 0x264
  let is_stereo = true
end)

module DD = Rpi.DMA.Make (struct
  let num = 5
end)

module DDAudio = Rpi.DMA.Make (struct
  let num = 4
end)

let with_buffer size fn =
  Printf.printf "+ %d\n%!" size;
  let mem_ref = X.mem_alloc ~size ~align:256 ~flags:0xc in
  let bus_addr = X.mem_lock mem_ref in
  let finally () =
    Printf.printf "- %d\n%!" size;
    X.mem_unlock mem_ref;
    X.mem_free mem_ref;
    Lwt.return_unit
  in
  Lwt.finalize (fun () -> fn bus_addr) finally

let led_pattern =
  let cut = 16 in
  List.init 361 (fun i ->
      if i / cut mod 2 = 0 then
        if i mod 2 = 1 && abs ((cut / 2) - (i mod cut)) < 3 * cut / 12 then
          { Ws2812b.r = 45; b = 50; g = 0 }
        else { r = 0; g = 0; b = 0 }
      else
        let mode = i / cut in
        let pos = abs ((cut / 2) - (i mod cut)) in
        { r = 0; g = 30 - pos; b = 30 + (mode / 2 * pos) })

let cycle pattern = List.tl pattern @ [ List.hd pattern ]
let stop = ref false

open Lwt.Syntax

let led_strip_main () =
  let* () = Lwt_unix.sleep 0.5 in
  let* ws = Ws2812b.init () in
  let size = Ws2812b.encoded_size ws 361 in
  Printf.printf "LED STORAGE: %d\n%!" size;

  with_buffer Rpi.DMA.Control_block.sizeof @@ fun control_block_addr ->
  with_buffer size @@ fun led_data_addr ->
  let control_block =
    mmap_cstruct
      (Rpi_devices.mem_bus_to_phys control_block_addr)
      (Nativeint.of_int Rpi.DMA.Control_block.sizeof)
  in
  let led_data =
    mmap_cstruct
      (Rpi_devices.mem_bus_to_phys led_data_addr)
      (Nativeint.of_int size)
  in

  Printf.printf "PWM\n%!";

  let ti =
    Rpi.DMA.Transfer_information.(
      empty
      |> set source_address_increment true
      |> set no_wide_bursts true
      |> set wait_write_response true
      |> set destination_dreq true |> set peripheral_mapping Pwm
      (* |> set destination_address_increment true *))
  in
  Rpi.DMA.Control_block.write control_block
    {
      transfer_information = ti;
      source_address = led_data_addr;
      (* FIF1 *)
      destination_address = Mem.(Rpi_devices.(peri_phys_to_bus pwm0) + 0x18n);
      transfer_length = size;
      stride = 0;
      next_control_address = 0;
    };

  let led_pattern = ref led_pattern in

  Printf.printf "LED> READY%!";
  let rec loop () =
    Printf.printf ">%!";
    Mtime.sleep_us_sync 1000L;
    Mem.dmb ();
    Ws2812b.write_cstruct ws led_data !led_pattern;
    Mem.dmb ();
    Mtime.sleep_us_sync 1000L;
    DD.Reg.Cs.(empty |> set reset true |> write);
    Mem.dmb ();
    Mtime.sleep_us_sync 10L;
    Mem.dmb ();
    DD.Reg.Cs.(empty |> set end' true |> set int true |> write);
    Mem.dmb ();
    Mtime.sleep_us_sync 10L;

    Mem.dmb ();
    DD.Reg.Conblk_ad.(
      empty |> set addr (control_block_addr |> Nativeint.to_int) |> write);
    DD.Reg.Cs.(
      empty |> set active true |> set wait_for_outstanding_writes true |> write);

    let rec wait_until_ready () =
      if
        (not (DD.Reg.Cs.(read end') || DD.Reg.Cs.(read error)))
        && DD.Reg.Cs.(read active)
      then (
        let* () = Lwt.pause () in
        Mem.dmb ();
        wait_until_ready ())
      else Lwt.return_unit
    in
    let* () = wait_until_ready () in

    if !stop then raise (Failure "stopped");
    led_pattern := cycle !led_pattern;

    if DD.Reg.Cs.(read end') then () else Printf.printf "\nno\n%!";
    loop ()
  in
  loop ()

let () = Sys.(set_signal sigint (Signal_handle (fun _ -> stop := true)))

let ( let+-+ ) (range, cut) fn =
  let rec aux pos =
    let* () = fn (pos, min (pos + cut) (range - 1)) in
    if pos + cut < range then aux (pos + cut) else Lwt.return_unit
  in
  aux 0

let play_music () =
  let music = Music.read "/papa_noel.bin" |> Option.get in
  let size = String.length music in

  let buffer_size = 1_00_000 in
  Printf.printf "Music %d\n%!" size;

  with_buffer Rpi.DMA.Control_block.sizeof @@ fun control_block_addr ->
  with_buffer buffer_size @@ fun music_data_addr ->
  Printf.printf "Setting up..\n%!";
  let control_block =
    mmap_cstruct
      (Rpi_devices.mem_bus_to_phys control_block_addr)
      (Nativeint.of_int Rpi.DMA.Control_block.sizeof)
  in
  let music_data =
    mmap_cstruct
      (Rpi_devices.mem_bus_to_phys music_data_addr)
      (Nativeint.of_int size)
  in
  Printf.printf "CB..\n%!";
  (* setup control block *)
  let ti =
    Rpi.DMA.Transfer_information.(
      empty
      |> set source_address_increment true
      |> set no_wide_bursts true
      |> set wait_write_response true
      |> set destination_dreq true |> set peripheral_mapping Dsi
      (* |> set destination_address_increment true *))
  in
  Rpi.DMA.Control_block.write control_block
    {
      transfer_information = ti;
      source_address = music_data_addr;
      (* FIF1 *)
      destination_address = Mem.(Rpi_devices.(peri_phys_to_bus pwm1) + 0x18n);
      transfer_length = buffer_size;
      stride = 0;
      next_control_address = 0;
    };
  Printf.printf "PWM..\n%!";
  (* PWM init *)
  let* _ = PwmAudio.init () in
  Printf.printf "Ready..\n%!";
  (* we cut the music in small pieces *)
  let+-+ start, stop' = (size, buffer_size / 4) in
  Printf.printf "%d => %d\n%!" start stop';
  (* load the piece *)
  for i = start to stop' - 1 do
    Cstruct.LE.set_uint32 music_data
      ((i - start) * 4)
      (Int32.of_int (Char.code music.[i]))
  done;
  (* DMA init *)
  Mem.dmb ();
  DDAudio.Reg.Cs.(empty |> set reset true |> write);
  DDAudio.Reg.Cs.(empty |> set end' true |> set int true |> write);
  Mem.dmb ();

  (* send the piece *)
  DDAudio.Reg.Conblk_ad.(
    empty |> set addr (control_block_addr |> Nativeint.to_int) |> write);
  DDAudio.Reg.Cs.(
    empty |> set active true |> set wait_for_outstanding_writes true |> write);

  (* wait for finish *)
  let rec loop () =
    if DDAudio.Reg.Cs.(read end') || DDAudio.Reg.Cs.(read error) || !stop then
      if !stop then raise (Failure "done") else Lwt.return_unit
    else
      let* () = Lwt.pause () in
      Mem.dmb ();
      loop ()
  in
  loop ()

let () =
  Mem.dmb ();
  (try Lwt_main.run (Lwt.join [ play_music (); led_strip_main () ])
   with Failure _ -> ());
  Mem.dmb ();

  Clock.kill ();
  DDAudio.Reg.Cs.(empty |> set reset true |> write);
  Printf.printf "The end.\n%!"
