
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdint.h>
#include <sys/mman.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

CAMLprim value caml_mmap(value caml_base, value caml_size)
{
    uint64_t base = Nativeint_val(caml_base);
    uint64_t size = Nativeint_val(caml_size);
    uint32_t pagemask = ~0UL ^ (getpagesize() - 1);
    uint32_t offsetmask = getpagesize() - 1;
    int mem_fd;
    void *mem;

    mem_fd = open("/dev/mem", O_RDWR | O_SYNC);
    if (mem_fd < 0)
    {
        perror("Can't open /dev/mem");
        return 0; 
    }

    mem = mmap(0, size, PROT_READ | PROT_WRITE, MAP_SHARED, mem_fd, base & pagemask);
    if (mem == MAP_FAILED)
    {
        perror("mmap error\n");
        return 0;
    }

    close(mem_fd);  

    return caml_copy_nativeint(((uint64_t)mem) + (base & offsetmask));
}