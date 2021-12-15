
#include <caml/mlvalues.h>

void cpu_wait_msec(value n) {
    register unsigned long f, t, r;
    // get the current counter frequency
    asm volatile ("mrs %0, cntfrq_el0" : "=r"(f));
    // read the current counter
    asm volatile ("mrs %0, cntpct_el0" : "=r"(t));
    // calculate expire value for counter
    t += ((f/1000)*Int_val(n))/1000;
    do { asm volatile ("mrs %0, cntpct_el0" : "=r"(r)); } while(r < t);
}