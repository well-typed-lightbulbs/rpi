val clock : Optint.t
val crystal_frequency : int
val gpio : Optint.t
val mtime : Optint.t
val pwm0 : Optint.t
val pwm1 : Optint.t
val aux : Optint.t
val uart0 : Optint.t
val armcore_irq_controller : Optint.t
val dma : int -> Optint.t
val mbox : Optint.t

val map_bigarray :
  Optint.t ->
  int ->
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val mbox_request :
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  unit
