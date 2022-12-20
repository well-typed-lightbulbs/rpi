val crystal_frequency : int
val clock : Optint.t
val clock_registers_size : int
val gpio : Optint.t
val gpio_registers_size : int
val mtime : Optint.t
val mtime_registers_size : int
val pwm0 : Optint.t
val pwm1 : Optint.t
val pwm_registers_size : int
val aux : Optint.t
val aux_register_size : int
val uart0 : Optint.t
val uart0_registers_size : int
val armcore_irq_controller : Optint.t
val armcore_irq_controller_registers_size : int
val dma : int -> Optint.t
val dma_registers_size : int
val mbox : Optint.t
val mbox_registers_size : int

(* translate bus address for memory to physical address *)
val mem_bus_to_phys : Optint.t -> Optint.t

(* translate phys address for peripheral to bus address *)
val peri_phys_to_bus : Optint.t -> Optint.t
