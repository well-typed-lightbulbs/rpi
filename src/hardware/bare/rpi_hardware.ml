include Rpi_devices

external ba_mmap :
  ('a, 'b) Stdlib.Bigarray.kind ->
  'c Stdlib.Bigarray.layout ->
  nativeint ->
  nativeint ->
  ('a, 'b, 'c) Stdlib.Bigarray.Genarray.t = "caml_ba_mmap"

let map_bigarray addr size =
  ba_mmap Bigarray.char Bigarray.c_layout
    (addr |> Optint.to_int |> Nativeint.of_int)
    (size |> Nativeint.of_int)
  |> Bigarray.array1_of_genarray

let mbox_request _ = failwith "MBOX: not implemented"
