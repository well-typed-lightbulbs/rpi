let offset v = Optint.(add Rpi_base.base (of_int v))
let crystal_frequency = Rpi_base.crystal_frequency
let clock = offset 0x101000
let clock_registers_size = 0xb0
let gpio = offset 0x200000
let gpio_registers_size = 0xa0
let mtime = offset 0x003000
let mtime_registers_size = 0x1c
let pwm0 = offset 0x20c000
let pwm1 = offset 0x20c800
let pwm_registers_size = 0x28
let aux = offset 0x215000
let aux_register_size = 0x10
let uart0 = offset 0x201000
let uart0_registers_size = 0x90
let armcore_irq_controller = offset 0xb000
let armcore_irq_controller_registers_size = 0x400

let dma = function
  | 15 -> offset 0xe05000
  | n when n >= 0 && n < 15 -> offset (0x7000 + (0x100 * n))
  | _ -> invalid_arg "Unknown DMA"

let dma_registers_size = 0x100
let mbox = offset 0xb880
let mbox_registers_size = 0x30
let mem_bus_to_phys v = Optint.logand v Optint.(lognot (of_int 0xC0000000))
let peri_bus_base = Optint.of_int 0x7e000000

let peri_phys_to_bus v =
  let ( + ) = Optint.add in
  let ( - ) = Optint.sub in
  v - Rpi_base.base + peri_bus_base
