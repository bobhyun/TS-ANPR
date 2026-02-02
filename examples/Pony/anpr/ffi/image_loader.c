// Image loader using stb_image
// The MIT License (MIT)
// Copyright © 2022-2025 TS-Solution Corp.

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"
#include "tsanpr_wrapper.h"
#include <stdlib.h>
#include <string.h>

// Load image and return pixel data
unsigned char* image_load(const char* filename, int* width, int* height, int* channels) {
    return stbi_load(filename, width, height, channels, 0);
}

// Load image with specific number of channels
unsigned char* image_load_channels(const char* filename, int* width, int* height, int desired_channels) {
    int channels;
    return stbi_load(filename, width, height, &channels, desired_channels);
}

// Free loaded image data
void image_free(unsigned char* data) {
    stbi_image_free(data);
}

// Load image and call ANPR directly (convenience function)
const char* image_anpr_read(const char* filename, const char* output_format, const char* options) {
    int width, height, channels;
    unsigned char* pixels = stbi_load(filename, &width, &height, &channels, 3);  // Force RGB
    if (pixels == NULL) {
        return NULL;
    }

    int stride = width * 3;
    const char* result = tsanpr_read_pixels(pixels, (unsigned long long)width,
                                            (unsigned long long)height, (long long)stride,
                                            "RGB", output_format, options);

    stbi_image_free(pixels);
    return result;
}
