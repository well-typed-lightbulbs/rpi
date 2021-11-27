#include <caml/bigarray.h>



extern unsigned char _binary_BCM4345C0_hcd_start[];
extern unsigned char _binary_BCM4345C0_hcd_size[];

CAMLprim value caml_bt_get_firmware(value unit) {
    long dims[1];
    dims[0] = (long)&_binary_BCM4345C0_hcd_size;

    return caml_ba_alloc(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, _binary_BCM4345C0_hcd_start, dims);
}