module Reg = Rpi.AUX.Reg

external irq_enable : unit -> unit = "irq_enable"

let stream, push = Lwt_stream.create ()
let read () = Lwt_stream.next stream
let state = Ke.Rke.create ~capacity:1024 Bigarray.Char

let handler _ =
  Mem.dmb ();
  while Reg.Mu_Iir.(read () && rx_fifo_not_empty) do
    let c = Reg.Mu_Io.(read () && data) in
    Ke.Rke.push state (Char.unsafe_chr c)
  done;
  Mem.dmb ();
  irq_enable ()

let rec restart_threads () =
  try
    let v = Ke.Rke.pop_exn state in
    push (Some v);
    restart_threads ()
  with Ke.Rke.Empty -> ()

let aux_irq_line = 29

let init () =
  (* set up interrupts. global initialization is already done by gilbraltar. *)
  Sys.set_signal aux_irq_line (Sys.Signal_handle handler);
  Reg.Mu_Ier.(empty |> set transmit_interrupt_enable true |> write)
