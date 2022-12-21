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


CAMLprim value caml_ba_mmap(value vkind, value vlayout, value caml_base, value caml_size)
{
    uint64_t base = Nativeint_val(caml_base);
    uint64_t size = Nativeint_val(caml_size);
    int flags = Caml_ba_kind_val(vkind) | Caml_ba_layout_val(vlayout);

    return caml_unix_mapped_alloc(flags, 1, (void*)base, &size);
}