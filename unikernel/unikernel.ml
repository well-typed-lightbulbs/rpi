open Rresult
open Lwt.Infix

let ( >>? ) = Lwt_result.bind

type color =
  { r : int; (* 8 bits *)
    g : int; (* 8 bits *)
    b : int; (* 8 bits *) }

let to_bits n =
  let rec aux acc n = function
    | 0 -> acc
    | i -> aux ((n mod 2 == 1) :: acc) (n lsr 1) (i - 1)
  in
  aux [] n 8

let color_to_bits { r; g; b } = to_bits g @ to_bits r @ to_bits b

let code = function
  | true  -> [ true; true; false ]
  | false -> [ true; false; false ]

let color_to_code color =
  color_to_bits color |> List.map code |> List.flatten

let red = { r = 10; g = 0; b = 0 }
let green = { r = 0; g = 10; b = 0 }
let blue = { r = 0; g = 0; b = 10 }
let black = { r = 0; g = 0; b = 0 }
let white = { r = 1; g = 1; b = 1 }

let pattern_to_code pat = List.map color_to_code pat |> List.flatten

let group code =
  let rec aux acc current_number = function
    | [], l ->
      List.rev (Int32.shift_left current_number (32 - l) :: acc)
    | rest, 32 -> aux (current_number :: acc) 0l (rest, 0)
    | true :: rest, i ->
        aux acc
          (let ( * ) = Int32.mul in
           let ( + ) = Int32.add in
           (current_number * 2l) + 1l)
          (rest, Int.(i + 1))
    | false :: rest, i ->
        aux acc
          (let ( * ) = Int32.mul in
           current_number * 2l)
          (rest, Int.(i + 1))
  in
  aux [] 0l (code, 0)

let reset = List.init 61 (fun _ -> black)

let pattern =
  List.init 61 @@ fun i ->
  { r = i; g = (i + 10) mod 30; b = (i + 20) mod 30 }

let next = function
  | first :: next -> next @ [ first ]
  | _ -> assert false

module Make
  (Console : Mirage_console.S)
  (Mtime : Rpi.Mtime.S)
  (Pwm : Rpi.Pwm.S) = struct

  let log console fmt = Fmt.kstr (fun str -> Console.log console str) fmt

  let blit src src_off dst dst_off len =
    Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len

  let line_of_queue queue =
    let exists ~p queue =
      let pos = ref 0 and res = ref (-1) in
      Ke.Rke.iter (fun chr -> if p chr && !res = -1 then res := !pos
                            ; incr pos) queue ;
      if !res = -1 then None else Some !res in
    match exists ~p:((=) '\n') queue with
    | None -> None
    | Some 0 -> Ke.Rke.N.shift_exn queue 1 ; Some ""
    | Some pos ->
      let tmp = Bytes.create pos in
      Ke.Rke.N.keep_exn queue ~blit ~length:Bytes.length ~off:0 ~len:pos tmp ;
      Ke.Rke.N.shift_exn queue (pos + 1) ;
      match Bytes.get tmp (pos - 1) with
      | '\r' -> Some (Bytes.sub_string tmp 0 (pos - 1))
      | _ -> Some (Bytes.unsafe_to_string tmp)
  
  let blit src src_off dst dst_off len =
    let src = Cstruct.to_bigarray src in
    Bigstringaf.blit src ~src_off dst ~dst_off ~len

  let rec getline flow queue = match line_of_queue queue with
    | Some line -> Lwt.return_ok (`Line line)
    | None ->
      Console.read flow >>= function
      | Ok `Eof -> Lwt.return_ok `Close
      | Ok (`Data v) ->
        Ke.Rke.N.push queue ~blit ~length:Cstruct.length ~off:0 v ;
        getline flow queue
      | Error err -> Lwt.return_error (R.msgf "%a" Console.pp_error err)

  let prompt = Cstruct.of_string "> "

  let run console =
    let rec go queue =
      Console.write console prompt 
      >|= R.reword_error (R.msgf "%a" Console.pp_write_error) >>? fun () ->
      getline console queue >>? function
      | `Close -> Lwt.return_ok ()
      | `Line line ->
        log console "# %S" line >>= fun () ->
        go queue in
    let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
    log console "Initialize the internal queue." >>= fun () ->
    go queue

  let output data =
    Pwm.init ();
    for i = 0 to Array.length data - 1 do
      Pwm.write data.(i)
    done;
    Pwm.stop ()

  let start console _ _ =
    log console "Hello World from MirageOS!" >>= fun () ->
    let rec infinite pattern =
      output
        (pattern_to_code pattern |> group |> List.map Int32.to_int
        |> Array.of_list);
      Mtime.sleep_us 10_000L ;
      infinite (next pattern) in
    infinite pattern
end
