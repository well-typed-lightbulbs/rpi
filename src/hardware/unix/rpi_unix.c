
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdint.h>
#include <sys/mman.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/bigarray.h"
#include "caml/memory.h"
#include "caml/fail.h"

value caml_unix_mapped_alloc(int flags, int num_dims, void * data, intnat * dim);

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
    printf("mmap: %016x\r", base);
    mem = mmap(0, size, PROT_READ | PROT_WRITE, MAP_SHARED, mem_fd, base & pagemask);
    printf("mmap: %016x => %016lx (%016lx)\n", base, mem, ((uint64_t)mem) + (base & offsetmask));
    if (mem == MAP_FAILED)
    {
        perror("mmap error\n");
        return 0;
    }

    close(mem_fd);

    return caml_copy_nativeint(((uint64_t)mem) + (base & offsetmask));
}



CAMLprim value caml_ba_mmap(value vkind, value vlayout, value caml_base, value caml_size)
{
    uint64_t base = Nativeint_val(caml_base);
    uint64_t size = Nativeint_val(caml_size);
    uint32_t pagemask = ~0UL ^ (getpagesize() - 1);
    uint32_t offsetmask = getpagesize() - 1;
    int mem_fd;
    void *mem;
    int flags = Caml_ba_kind_val(vkind) | Caml_ba_layout_val(vlayout);

    mem_fd = open("/dev/mem", O_RDWR | O_SYNC);
    if (mem_fd < 0)
    {
        perror("Can't open /dev/mem");
        return 0;

    }
    printf("mmap: %016x\n", base);
    mem = mmap(0, size, PROT_READ | PROT_WRITE, MAP_SHARED, mem_fd, base & pagemask);
    if (mem == MAP_FAILED)
    {
        perror("mmap error\n");
        return 0;
    }

    close(mem_fd);


    return caml_unix_mapped_alloc(flags, 1, (void*)((uint64_t)mem) + (base & offsetmask), &size);
}

#include <sys/ioctl.h>
#include <sys/sysmacros.h>
#include <sys/stat.h>


CAMLprim value caml_vcio_open() {
    int file_desc = open("/dev/vcio", 0);
    if (file_desc >= 0) {
        return Val_int(file_desc);
    }

    caml_failwith("caml_vcio_open");
}

#define MAJOR_NUM 100
#define IOCTL_MBOX_PROPERTY _IOWR(MAJOR_NUM, 0, char *)

CAMLprim value caml_vcio_write(value fd, value buf) {
  CAMLparam2 (fd, buf);

  int fd_ = Int_val(fd);

  char* ptr = Caml_ba_data_val(buf);

  int ret_val = ioctl(fd_, IOCTL_MBOX_PROPERTY, ptr);

  if (ret_val < 0) {
      perror("ioctl_set_msg failed\n");
  }

  CAMLreturn (Val_unit);
}
