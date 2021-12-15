module Handle = struct
  type t = { id : < >; check_ready : unit -> bool }

  let register ~check_ready = { id = object end; check_ready }

  let compare a b = Stdlib.compare a.id b.id
end

module HandleMap = Map.Make (Handle)

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

let rec go t =
  Lwt.wakeup_paused ();
  Time.restart_threads Time.now;
  match Lwt.poll t with
  | Some () -> ()
  | None ->
      (let timeout =
         match Time.select_next () with
         | None -> Int64.add (Rpi.Mtime.elapsed_us ()) (Duration.of_day 1)
         | Some tm -> tm
       in
       let ready_set = yield timeout in
       match ready_set with
       | [] -> ()
       | ready_set ->
           List.iter
             (fun h ->
               let condition = HandleMap.find h !work in
               work := HandleMap.remove h !work;
               Lwt_condition.broadcast condition ())
             ready_set);
      go t

let () = at_exit @@ fun () -> Lwt.abandon_wakeups ()

module Time = Time
