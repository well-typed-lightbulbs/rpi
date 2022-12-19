
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include <stdint.h>
#include <errno.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <sys/sysmacros.h>
#include <sys/stat.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/bigarray.h"

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

  value buffer = Field(buf, 0);
  int off = Int_val(Field(buf, 1));
  int len = Int_val(Field(buf, 2));
  int fd_ = Int_val(fd);

  char* ptr = Caml_ba_data_val(buffer);

  int ret_val = ioctl(fd_, IOCTL_MBOX_PROPERTY, ptr);

  if (ret_val < 0) {
      perror("ioctl_set_msg failed\n");
  }

  CAMLreturn (Val_unit);
}
