let offset = Nativeint.add Rpi_base.base
let crystal_frequency = Rpi_base.crystal_frequency
let clock = offset 0x101000n
let clock_registers_size = 0xb0n
let gpio = offset 0x200000n
let gpio_registers_size = 0xa0n
let mtime = offset 0x003000n
let mtime_registers_size = 0x1cn
let pwm0 = offset 0x20c000n
let pwm1 = offset 0x20c800n
let pwm_registers_size = 0x28n
let aux = offset 0x215000n
let aux_register_sizes = 0x100
let uart0 = offset 0x201000n
let uart0_registers_size = 0x90n
let armcore_irq_controller = offset 0xb000n
let armcore_irq_controller_registers_size = 0x400n

let dma = function
  | 15 -> offset 0xe05000n
  | n when n >= 0 && n < 15 ->
      Nativeint.(add (offset 0x7000n) (mul 0x100n (of_int n)))
  | _ -> invalid_arg "Unknown DMA"

let dma_registers_size = 0x100n
let mbox = offset 0xb880n
let mbox_registers_size = 0x30n
let bus_to_phys v = Nativeint.logand v (Nativeint.lognot 0xC0000000n)
