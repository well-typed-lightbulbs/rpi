let base = Rpi_hardware.gpio

(* GPIO addresses *)

let gp_sel0 = Mem.(base + 0x00n)

let gp_set0 = Mem.(base + 0x1Cn)

let gp_set1 = Mem.(base + 0x20n)

let gp_clr0 = Mem.(base + 0x28n)

let gp_clr1 = Mem.(base + 0x2Cn)

let gp_lev0 = Mem.(base + 0x34n)

let gp_lev1 = Mem.(base + 0x38n)

let gp_pud = Mem.(base + 0x94n)

let gp_pudclk0 = Mem.(base + 0x98n)

let gp_pudclk1 = Mem.(base + 0x98n)

(* Pins *)
type pin = P00 | P01 | P02 | P03 | P04 | P05 | P06 | P07 | P08 | P09 | P10 
  | P11 | P12 | P13 | P14 | P15 | P16 | P17 | P18 | P19 | P20 | P21 | P22 | P23
  | P24 | P25 | P26 | P27 | P28 | P29 | P30 | P31 | P32 | P33 | P34 | P35 | P36
  | P37 | P38 | P39 | P40 | P41 | P42 | P43 | P44 | P45 | P46 | P47 | P48 | P49
  | P50 | P51 | P52 | P53
[@@ocamlformat "disable"]

let pin_to_int : pin -> int = fun p -> Obj.magic (Obj.repr p)

(* Setup *)

type func = F_IN | F_OUT | F_ALT5 | F_ALT4 | F_ALT0 | F_ALT1 | F_ALT2 | F_ALT3

let func_to_int : func -> int = fun f -> Obj.magic (Obj.repr f)

let set_func p f =
  let p = pin_to_int p in
  let f = func_to_int f in
  let r = Mem.offset gp_sel0 (4 * (p / 10)) in
  let bit_start = p mod 10 * 3 in
  Mem.set_int_bits r ~bits:(0b111 lsl bit_start) (f lsl bit_start);
  ()

type pull_state = PULL_OFF | PULL_DOWN | PULL_UP

let pull_state_to_int : pull_state -> int = fun s -> Obj.magic (Obj.repr s)

let set_pull_state p s =
  let p = pin_to_int p in
  let s = pull_state_to_int s in
  let clk, n = if p > 31 then (gp_pudclk1, p land 31) else (gp_pudclk0, p) in
  Mem.set_int gp_pud s;
  Mem.wait 150;
  Mem.set_int clk (1 lsl n);
  Mem.wait 150;
  Mem.set_int gp_pud 0;
  Mem.set_int clk 0;
  ()

(* Read and write *)

let set p b =
  let p = pin_to_int p in
  let r, n =
    if p > 31 then ((if b then gp_set1 else gp_clr1), p land 31)
    else ((if b then gp_set0 else gp_clr0), p)
  in
  Mem.set_int r (1 lsl n);
  ()

let get p =
  let p = pin_to_int p in
  let r, n =
    if p > 31 then (gp_lev1, p land 31)
    else (gp_lev0, p)
  in
  let i = Mem.get_int r in
  i land (1 lsl n) <> 0
