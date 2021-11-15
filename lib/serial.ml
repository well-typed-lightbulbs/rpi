(* We define serial as soon as possible so that it's available for
   debugging in this module. *)
(* UART0 registers *)

let uart_base = Mem.(Mmio.base + 0x215000n) (*aux base*)

let aux_irq = Mem.(uart_base + 0x00n)

let aux_enables = Mem.(uart_base + 0x04n)

let aux_mu_io_reg = Mem.(uart_base + 0x40n)

let aux_mu_ier_reg = Mem.(uart_base + 0x44n)

let aux_mu_iir_reg = Mem.(uart_base + 0x48n)

let aux_mu_lcr_reg = Mem.(uart_base + 0x4cn)

let aux_mu_mcr_reg = Mem.(uart_base + 0x50n)

let aux_mu_lsr_reg = Mem.(uart_base + 0x54n)

let aux_mu_msr_reg = Mem.(uart_base + 0x58n)

let aux_mu_scratch = Mem.(uart_base + 0x5cn)

let aux_mu_cntl_reg = Mem.(uart_base + 0x60n)

let aux_mu_stat_reg = Mem.(uart_base + 0x64n)

let aux_mu_baud_reg = Mem.(uart_base + 0x68n)

let aux_uart_clock = 500000000

let uart_max_queue = 16 * 1024

(* Initialisation *)

let inited = ref false

let aux_mu_baud baud = (aux_uart_clock / (baud * 8)) - 1

let init () =
  if !inited then ()
  else (
    (* Disable UART *)
    Mem.set_int aux_enables 1;
    Mem.set_int aux_mu_ier_reg 0;
    Mem.set_int aux_mu_cntl_reg 0;
    Mem.set_int aux_mu_lcr_reg 3;
    (*8 bits*)
    Mem.set_int aux_mu_mcr_reg 0;
    Mem.set_int aux_mu_ier_reg 0;
    Mem.set_int aux_mu_iir_reg 0xc6;
    Mem.set_int aux_mu_baud_reg (aux_mu_baud 115200);
    Gpio.(set_pull_state P14 PULL_OFF);
    Gpio.(set_func P14 F_ALT5);
    Gpio.(set_pull_state P15 PULL_OFF);
    Gpio.(set_func P15 F_ALT5);

    Mem.set_int aux_mu_cntl_reg 3;
    inited := true)

(* Reads *)
(*
   let read_byte () =
     while (Mem.get_int aux land (1 lsl 4) <> 0) do () done;
     Mem.get_int uart_dr land 0xFF

   let try_read_byte () = match Mem.get_int uart_fr land (1 lsl 4) with
   | 0 -> None
   | n -> Some (Mem.get_int uart_dr land 0xFF) *)

(* Writes *)

let write_byte byte =
  while Mem.get_int aux_mu_lsr_reg land 0x20 == 0 do
    ()
  done;
  Mem.set_int aux_mu_io_reg byte

let write s =
  let max = String.length s - 1 in
  for i = 0 to max do
    write_byte (Char.code (String.unsafe_get s i))
  done

let b = Buffer.create 256

let ppf = Format.formatter_of_buffer b

let writef fmt =
  let k ppf =
    Format.pp_print_flush ppf ();
    let c = Buffer.contents b in
    Buffer.clear b;
    write c
  in
  Format.kfprintf k ppf fmt
