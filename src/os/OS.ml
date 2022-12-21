
external wfi : unit -> unit = "wfi"

external irq_enable : unit -> unit = "irq_enable"

module Handle = struct
  type t = { id : < >; check_ready : unit -> bool }

  let register ~check_ready = { id = object end; check_ready }

  let compare a b = Stdlib.compare a.id b.id
end

module HandleMap = Map.Make (Handle)

module Hooks = struct

  let state = ref []

  let register callback =
    state := callback :: !state

  let iter () =
    List.iter (fun fn -> fn ()) !state
end

let rec run t =
  Lwt.wakeup_paused ();
  Time.restart_threads Time.now;
  Hooks.iter ();
  match Lwt.poll t with
  | Some () -> ()
  | None ->
      let timeout =
        match Time.select_next () with
        | None -> Int64.add (Rpi.Mtime.elapsed_us ()) (Duration.of_day 1)
        | Some tm -> tm
      in
      Mtime.schedule_next_interrupt L1 timeout;
      wfi ();
      Bytes.create 1 |> ignore;
      run t

let timer_interrupt_handler _ =
  Mtime.acknowledge_interrupt L1;
  Mem.dmb ();
  irq_enable ()

let go t =
  Sys.set_signal
    (Mtime.(interrupt_line_to_signal_number L1))
    (Sys.Signal_handle timer_interrupt_handler);
  run t

let () = at_exit @@ fun () -> Lwt.abandon_wakeups ()

module Time = Time
