/*
 * stb_image OCaml stubs
 * Provides OCaml bindings for stb_image functions
 */

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>

/*
 * stbi_load_wrapper: Load an image file and return pixel data as bigarray
 * Returns: (bigarray, width, height, channels) tuple or raises exception
 */
CAMLprim value stbi_load_wrapper(value filename_val, value desired_channels_val) {
    CAMLparam2(filename_val, desired_channels_val);
    CAMLlocal2(result, ba);

    const char *filename = String_val(filename_val);
    int desired_channels = Int_val(desired_channels_val);
    int width, height, channels;

    unsigned char *data = stbi_load(filename, &width, &height, &channels, desired_channels);

    if (data == NULL) {
        caml_failwith(stbi_failure_reason());
    }

    /* If desired_channels is specified (non-zero), use it; otherwise use actual channels */
    int actual_channels = (desired_channels > 0) ? desired_channels : channels;
    int size = width * height * actual_channels;

    /* Create a char bigarray that owns the data */
    intnat dims[1] = { size };
    ba = caml_ba_alloc(CAML_BA_CHAR | CAML_BA_C_LAYOUT | CAML_BA_MANAGED, 1, NULL, dims);

    /* Copy data to the bigarray (we need to copy because OCaml GC manages the bigarray) */
    memcpy(Caml_ba_data_val(ba), data, size);

    /* Free the original stbi data */
    stbi_image_free(data);

    /* Return tuple: (bigarray, width, height, channels) */
    result = caml_alloc_tuple(4);
    Store_field(result, 0, ba);
    Store_field(result, 1, Val_int(width));
    Store_field(result, 2, Val_int(height));
    Store_field(result, 3, Val_int(actual_channels));

    CAMLreturn(result);
}
