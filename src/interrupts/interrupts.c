#include <stddef.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>

extern void irq_disable();
extern int current_el();
extern void irq_wfi();
extern void uart_drain_output_queue();
extern void uart_write_text(char*);

unsigned char uart_read_byte();

value irq = 0;

void irq_setup(value value) {
    irq = value;
}

void handle_irq() {
    uart_write_text("IRQ!\n");
    uart_drain_output_queue();
    Store_field(irq, 0, Val_int(1));
    irq_disable();
}

value caml_current_el() {
    return Val_int(current_el());
}


void do_something() {
    uart_read_byte();
}