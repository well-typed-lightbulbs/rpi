external mmap : nativeint -> nativeint -> nativeint = "caml_mmap"

external ba_mmap :
  ('a, 'b) Stdlib.Bigarray.kind ->
  'c Stdlib.Bigarray.layout ->
  nativeint ->
  nativeint ->
  ('a, 'b, 'c) Stdlib.Bigarray.Genarray.t = "caml_ba_mmap"

open Rpi_devices

let mmap a b =
  mmap (Optint.to_int a |> Nativeint.of_int) (Nativeint.of_int b)
  |> Nativeint.to_int |> Optint.of_int

let crystal_frequency = crystal_frequency
let uart0 = mmap uart0 uart0_registers_size
let pwm0 = mmap pwm0 pwm_registers_size
let pwm1 = mmap pwm1 pwm_registers_size
let aux = mmap aux pwm_registers_size
let mtime = mmap mtime mtime_registers_size
let gpio = mmap gpio gpio_registers_size
let clock = mmap clock clock_registers_size

let armcore_irq_controller =
  mmap armcore_irq_controller armcore_irq_controller_registers_size

let dma n = mmap (dma n) dma_registers_size
let mbox = mmap mbox mbox_registers_size

let map_bigarray addr size =
  ba_mmap Bigarray.char Bigarray.c_layout
    (addr |> Optint.to_int |> Nativeint.of_int)
    (size |> Nativeint.of_int)
  |> Bigarray.array1_of_genarray

type file_descr

external vcio_open : unit -> file_descr = "caml_vcio_open"

external vcio_write :
  file_descr ->
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  unit = "caml_vcio_write"

(* TODO: close ?*)
let mbox_fd = vcio_open ()
let mbox_request = vcio_write mbox_fd