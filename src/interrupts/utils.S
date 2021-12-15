.globl irq_init_vectors
irq_init_vectors:
    adr x0, vectors
    msr vbar_el1, x0
    ret

.globl irq_enable
irq_enable:
    msr daifclr, #0x2
    ret

.globl irq_disable
irq_disable:
    msr daifset, #0x2
    ret

.globl irq_wfi
irq_wfi:
    wfe
    ret

.globl current_el
current_el:
    mrs x0, CurrentEL
    lsr x0, x0, #2
    ret
