let clock = Nativeint.add Rpi_base.base 0x101000n

let clock_registers_size = 0xb0n

let gpio = Nativeint.add Rpi_base.base 0x200000n

let gpio_registers_size = 0xa0n

let mtime = Nativeint.add Rpi_base.base 0x003000n

let mtime_registers_size = 0x1cn

let pwm = Nativeint.add Rpi_base.base 0x20c000n

let pwm_registers_size = 0x28n

let uart0 = Nativeint.add Rpi_base.base 0x201000n

let uart0_registers_size = 0x90n
