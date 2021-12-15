external irq_init_vectors : unit -> unit = "irq_init_vectors" [@@noalloc]

external irq_enable : unit -> unit = "irq_enable" [@@noalloc]

external irq_disable : unit -> unit = "irq_disable" [@@noalloc]

external irq_setup : bool ref -> unit = "irq_setup" [@@noalloc]

external wfi : unit -> unit = "irq_wfi" [@@noalloc]

let irq = ref false

let init () =
  Printf.printf "irq_setup\n%!";
  irq_setup irq;
  Printf.printf "irq_init_vectors\n%!";
  irq_init_vectors ();
  Printf.printf "irq_enable\n%!";
  irq_enable ()

let enable_irqs () =
  Mem.set_int Armcore.Reg.IRQ0_set_en_0.addr (1 lsl 29);
  (* bit 29 is aux. *)
  Mem.set_int Armcore.Reg.IRQ0_set_en_1.addr 0

let dispatch () =
  let int_nul = Mem.get_int Armcore.Reg.IRQ0_set_en_0.addr in
  Printf.printf "%08x\n%!" int_nul;
  let int0 = Mem.get_int Armcore.Reg.IRQ0_pending_0.addr in
  Printf.printf "%08x\n%!" int0
