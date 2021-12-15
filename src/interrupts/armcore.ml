let base = Rpi_hardware.armcore_irq_controller

module Reg = struct
  module IRQ0_pending_0 = struct
    let addr = Mem.(base + 0x200n)
  end

  module IRQ0_pending_1 = struct
    let addr = Mem.(base + 0x204n)
  end

  module IRQ0_pending_2 = struct
    let addr = Mem.(base + 0x208n)
  end

  module IRQ0_set_en_0 = struct
    let addr = Mem.(base + 0x210n)
  end

  module IRQ0_set_en_1 = struct
    let addr = Mem.(base + 0x214n)
  end

  module IRQ0_set_en_2 = struct
    let addr = Mem.(base + 0x218n)
  end

  module IRQ0_clr_en_0 = struct
    let addr = Mem.(base + 0x220n)
  end

  module IRQ0_clr_en_1 = struct
    let addr = Mem.(base + 0x224n)
  end

  module IRQ0_clr_en_2 = struct
    let addr = Mem.(base + 0x228n)
  end
end
