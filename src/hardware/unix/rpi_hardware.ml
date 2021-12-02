external mmap : nativeint -> nativeint -> nativeint = "caml_mmap"

open Rpi_devices

let uart0 = mmap uart0 uart0_registers_size

let pwm = mmap pwm pwm_registers_size

let mtime = mmap mtime mtime_registers_size

let gpio = mmap gpio gpio_registers_size

let clock = mmap clock clock_registers_size
