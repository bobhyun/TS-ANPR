/*
 * stb_image shared library implementation
 * Compile with: gcc -shared -fPIC -O2 stb_image_lib.c -o libstb_image.so (Linux)
 *               cl /LD /O2 stb_image_lib.c /Fe:stb_image.dll (Windows)
 */
#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

#ifdef _WIN32
#define EXPORT __declspec(dllexport)
#else
#define EXPORT __attribute__((visibility("default")))
#endif

EXPORT unsigned char* stbi_load_export(const char* filename, int* x, int* y, int* channels_in_file, int desired_channels) {
    return stbi_load(filename, x, y, channels_in_file, desired_channels);
}

EXPORT void stbi_image_free_export(void* retval_from_stbi_load) {
    stbi_image_free(retval_from_stbi_load);
}
