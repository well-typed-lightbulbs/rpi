external mmap : nativeint -> nativeint -> nativeint = "caml_mmap"

open Rpi_devices

let uart0 = mmap uart0 uart0_registers_size

let pwm0 = mmap pwm0 pwm_registers_size

let pwm1 = mmap pwm1 pwm_registers_size

let mtime = mmap mtime mtime_registers_size

let gpio = mmap gpio gpio_registers_size

let clock = mmap clock clock_registers_size

let armcore_irq_controller =
  mmap armcore_irq_controller armcore_irq_controller_registers_size
