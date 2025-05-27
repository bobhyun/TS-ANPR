/**
 * The MIT License (MIT)
 * Copyright © 2022-2025 TS-Solution Corp.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to all conditions.
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 **/

#pragma once

#ifdef _WIN32
#define _CRT_SECURE_NO_WARNINGS
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#define WINAPI
#endif

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct
    {
        const char *(WINAPI *anpr_initialize)(const char *mode); // [IN] Sets the library operation mode
        const char *(WINAPI *anpr_read_file)(
            const char *imgFileName,  // [IN] Input image file name
            const char *outputFormat, // [IN] Output data format
            const char *options);     // [IN] Feature options
        const char *(WINAPI *anpr_read_pixels)(
            const unsigned char *pixels, // [IN] Starting address of the image pixels
            const unsigned long width,   // [IN] Number of pixels in image width
            const unsigned long height,  // [IN] Number of pixels in image height
            const long stride,           // [IN] Number of bytes per image line
            const char *pixelFormat,     // [IN] Image pixel format
            const char *outputFormat,    // [IN] Output data format
            const char *options);        // [IN] Feature options
    } TSANPR;

#ifdef _WIN32
    int TSANPR_load(TSANPR *tsanpr, const wchar_t *engineFileName);
#else
    int TSANPR_load(TSANPR *tsanpr, const char *engineFileName);
#endif
    void TSANPR_unload();

#ifdef __cplusplus
};
#endif // __cplusplus
