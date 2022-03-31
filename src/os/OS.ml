
external irq_enable : unit -> unit = "irq_enable"

module Handle = struct
  type t = { id : < >; check_ready : unit -> bool }

  let register ~check_ready = { id = object end; check_ready }

  let compare a b = Stdlib.compare a.id b.id
end

module HandleMap = Map.Make (Handle)

(*
let work = ref HandleMap.empty

let wait_for h =
  match HandleMap.find h !work with
  | exception Not_found ->
      let cond = Lwt_condition.create () in
      work := HandleMap.add h cond !work;
      Lwt_condition.wait cond
  | cond -> Lwt_condition.wait cond

let rec yield timeout =
  let ready_set =
    HandleMap.bindings !work
    |> List.filter_map (fun (({ Handle.check_ready; _ } as h), _) ->
           if check_ready () then Some h else None)
  in
  match ready_set with
  | [] when Int64.compare (Rpi.Mtime.elapsed_us ()) timeout > 0 -> []
  | [] ->
      Rpi.Mtime.sleep_us 1000L;
      yield timeout
  | ready -> ready
*)
(*
module UART = struct
  

  let readers = ref None

  let read_char_rx_fifo () = Some 'c'

  let rec handle _ =
    match read_char_rx_fifo (), !readers with
    | None, _ -> enable_global_interrupts ()
    | _, None -> enable_global_interrupts ()
    | Some c, Some r -> 
      Lwt.wakeup r c; 
      handle 0

  let enable_interrupts () = 
    Sys.set_signal 11 (Sys.Signal_handle handle)

  let read () =
    enable_interrupts ();
    let promise, resolver = Lwt.task () in
    readers := Some resolver;
    promise
    

end
*)

(*
let rec go t =
  Lwt.wakeup_paused ();
  Time.restart_threads Time.now;
  match Lwt.poll t with
  | Some () -> ()
  | None ->
    let timeout =
      match Time.select_next () with
      | None -> Int64.add (Rpi.Mtime.elapsed_us ()) (Duration.of_day 1)
      | Some tm -> tm
    in
    timer_schedule_next_interrupt timeout;
    wait_for_interrupt ();
    Bytes.create 0 |> ignore; (* poll signals *)
    go t
*)

external wfi : unit -> unit = "wfi"

let rec run t =
  Lwt.wakeup_paused ();
  Time.restart_threads Time.now;
  match Lwt.poll t with
  | Some () -> ()
  | None ->
      let timeout =
        match Time.select_next () with
        | None -> Int64.add (Rpi.Mtime.elapsed_us ()) (Duration.of_day 1)
        | Some tm -> tm
      in
      Rpi.Mtime.schedule_next_interrupt L1 timeout;
      wfi ();
      print_endline "hello";
      Bytes.create 1 |> ignore;
      Printf.printf "Got interrupt\n%!";
      run t

let timer_interrupt_handler _ =
  Printf.printf "I am handler\n%!";
  Rpi.Mtime.acknowledge_interrupt L1;
  Mem.dmb ();
  irq_enable ()

let go t = 
  Sys.set_signal 
    (Rpi.Mtime.(interrupt_line_to_signal_number L1)) 
    (Sys.Signal_handle timer_interrupt_handler);
  run t

let () = at_exit @@ fun () -> Lwt.abandon_wakeups ()

module Time = Time
