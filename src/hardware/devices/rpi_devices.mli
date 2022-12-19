val crystal_frequency : int
val clock : nativeint
val clock_registers_size : nativeint
val gpio : nativeint
val gpio_registers_size : nativeint
val mtime : nativeint
val mtime_registers_size : nativeint
val pwm0 : nativeint
val pwm1 : nativeint
val pwm_registers_size : nativeint
val aux : nativeint
val aux_register_sizes : int
val uart0 : nativeint
val uart0_registers_size : nativeint
val armcore_irq_controller : nativeint
val armcore_irq_controller_registers_size : nativeint
val dma : int -> nativeint
val dma_registers_size : nativeint
val mbox : nativeint
val mbox_registers_size : nativeint

(* translate bus address for memory to physical address *)
val mem_bus_to_phys : nativeint -> nativeint

(* translate phys address for peripheral to bus address *)
val peri_phys_to_bus : nativeint -> nativeint
