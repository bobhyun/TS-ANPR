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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tsanpr.h"
#include <png.h>
#include <jpeglib.h>
#include <ctype.h>

TSANPR tsanpr;

static const char *examplesBaseDir = "../..";

// Generate engine filename depending on platform
const char *getEngineFileName()
{
    static char buffer[512];
#ifdef _WIN32
#ifdef _WIN64
    // 64-bit Windows
    snprintf(buffer, sizeof(buffer), "%s\\bin\\windows-x86_64\\tsanpr.dll", examplesBaseDir);
#else
    // 32-bit Windows
    snprintf(buffer, sizeof(buffer), "%s\\bin\\windows-x86\\tsanpr.dll", examplesBaseDir);
#endif
#elif defined(__x86_64__) || defined(_M_X64)
    // 64-bit Linux
    snprintf(buffer, sizeof(buffer), "%s/bin/linux-x86_64/libtsanpr.so", examplesBaseDir);
#elif defined(__aarch64__)
    // 64-bit ARM Linux
    snprintf(buffer, sizeof(buffer), "%s/bin/linux-aarch64/libtsanpr.so", examplesBaseDir);
#else
    buffer[0] = 0;
#endif
    return buffer;
}

// Convert UTF-8 string to wchar_t* on Windows
#ifdef _WIN32
#include <windows.h>
wchar_t *to_wstring(const char *str)
{
    int len = MultiByteToWideChar(CP_UTF8, 0, str, -1, NULL, 0);
    wchar_t *wstr = (wchar_t *)malloc(len * sizeof(wchar_t));
    MultiByteToWideChar(CP_UTF8, 0, str, -1, wstr, len);
    // Remove trailing null character
    if (len > 1 && wstr[len - 1] == L'\0')
        wstr[len - 1] = 0;
    return wstr;
}
#endif

// Read image file using anpr_read_file
void readImageFile(const char *imgfile, const char *outputFormat, const char *options)
{
    printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options);
    const char *result = tsanpr.anpr_read_file(imgfile, outputFormat, options);
    printf("%s\n", result);
}

// Read encoded image as binary and call anpr_read_pixels
void readEncodedImage(const char *imgfile, const char *outputFormat, const char *options)
{
    printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options);
    FILE *file = fopen(imgfile, "rb");
    if (!file)
    {
        printf("File open failed\n");
        return;
    }
    fseek(file, 0, SEEK_END);
    long filesize = ftell(file);
    fseek(file, 0, SEEK_SET);
    if (filesize <= 0)
    {
        printf("File size invalid or zero\n");
        fclose(file);
        return;
    }
    unsigned char *encodedImg = (unsigned char *)malloc(filesize);
    if (!encodedImg)
    {
        fclose(file);
        printf("Memory allocation failed\n");
        return;
    }
    size_t nread = fread(encodedImg, 1, filesize, file);
    if (nread != (size_t)filesize)
    {
        printf("File read error: expected %ld bytes, got %zu bytes\n", filesize, nread);
        free(encodedImg);
        fclose(file);
        return;
    }
    fclose(file);

    const char *result = tsanpr.anpr_read_pixels(
        encodedImg,
        filesize,
        0,
        0,
        "encoded",
        outputFormat,
        options);
    printf("%s\n", result);
    free(encodedImg);
}

// Convert file extension to lowercase and compare
int get_lowercase_ext(const char *filename, char *extbuf, size_t extbufsize)
{
    const char *dot = strrchr(filename, '.');
    if (!dot || strlen(dot) < 2)
        return 0; // No extension
    size_t len = strlen(dot + 1);
    if (len + 1 > extbufsize)
        len = extbufsize - 1;
    for (size_t i = 0; i < len; ++i)
        extbuf[i] = (char)tolower((unsigned char)dot[1 + i]);
    extbuf[len] = '\0';
    return 1;
}

// Read PNG file using libpng
int read_png(const char *filename, unsigned char **out_pixels, int *out_width, int *out_height, int *out_channels)
{
    FILE *fp = fopen(filename, "rb");
    if (!fp)
        return -1;

    png_structp png = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
    if (!png)
    {
        fclose(fp);
        return -2;
    }
    png_infop info = png_create_info_struct(png);
    if (!info)
    {
        png_destroy_read_struct(&png, NULL, NULL);
        fclose(fp);
        return -3;
    }

    if (setjmp(png_jmpbuf(png)))
    {
        png_destroy_read_struct(&png, &info, NULL);
        fclose(fp);
        return -4;
    }

    png_init_io(png, fp);
    png_read_info(png, info);

    int width = png_get_image_width(png, info);
    int height = png_get_image_height(png, info);
    int color_type = png_get_color_type(png, info);
    int bit_depth = png_get_bit_depth(png, info);

    // Convert palette/gray/etc. to 8-bit RGBA
    if (bit_depth == 16)
        png_set_strip_16(png);
    if (color_type == PNG_COLOR_TYPE_PALETTE)
        png_set_palette_to_rgb(png);
    if (color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8)
        png_set_expand_gray_1_2_4_to_8(png);
    if (png_get_valid(png, info, PNG_INFO_tRNS))
        png_set_tRNS_to_alpha(png);
    if (color_type == PNG_COLOR_TYPE_RGB ||
        color_type == PNG_COLOR_TYPE_GRAY ||
        color_type == PNG_COLOR_TYPE_PALETTE)
        png_set_filler(png, 0xFF, PNG_FILLER_AFTER);
    if (color_type == PNG_COLOR_TYPE_GRAY ||
        color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
        png_set_gray_to_rgb(png);

    png_read_update_info(png, info);

    int channels = png_get_channels(png, info);
    int rowbytes = (int)png_get_rowbytes(png, info);
    unsigned char *pixels = (unsigned char *)malloc(rowbytes * height);
    if (!pixels)
    {
        png_destroy_read_struct(&png, &info, NULL);
        fclose(fp);
        return -5;
    }

    png_bytep *row_pointers = (png_bytep *)malloc(sizeof(png_bytep) * height);
    if (!row_pointers)
    {
        free(pixels);
        png_destroy_read_struct(&png, &info, NULL);
        fclose(fp);
        return -6;
    }
    for (int y = 0; y < height; y++)
        row_pointers[y] = pixels + y * rowbytes;

    png_read_image(png, row_pointers);

    fclose(fp);
    png_destroy_read_struct(&png, &info, NULL);
    free(row_pointers);

    *out_pixels = pixels;
    *out_width = width;
    *out_height = height;
    *out_channels = channels;
    return rowbytes;
}

// Read JPEG file using libjpeg
int read_jpeg(const char *filename, unsigned char **out_pixels, int *out_width, int *out_height, int *out_channels)
{
    FILE *fp = fopen(filename, "rb");
    if (!fp)
        return -1;

    struct jpeg_decompress_struct cinfo;
    struct jpeg_error_mgr jerr;
    cinfo.err = jpeg_std_error(&jerr);
    jpeg_create_decompress(&cinfo);
    jpeg_stdio_src(&cinfo, fp);
    jpeg_read_header(&cinfo, TRUE);
    jpeg_start_decompress(&cinfo);

    int width = cinfo.output_width;
    int height = cinfo.output_height;
    int channels = cinfo.output_components;
    int row_stride = width * channels;
    unsigned char *pixels = (unsigned char *)malloc(row_stride * height);
    if (!pixels)
    {
        jpeg_destroy_decompress(&cinfo);
        fclose(fp);
        return -2;
    }

    while (cinfo.output_scanline < cinfo.output_height)
    {
        unsigned char *rowptr = pixels + cinfo.output_scanline * row_stride;
        jpeg_read_scanlines(&cinfo, &rowptr, 1);
    }

    jpeg_finish_decompress(&cinfo);
    jpeg_destroy_decompress(&cinfo);
    fclose(fp);

    *out_pixels = pixels;
    *out_width = width;
    *out_height = height;
    *out_channels = channels;
    return row_stride;
}

// Get pixel format string for tsanpr based on channels
const char *getPixelFormat(int channels)
{
    if (channels == 1)
        return "GRAY";
    else if (channels == 2)
        return "BGR565";
    else if (channels == 3)
        return "BGR";
    else if (channels == 4)
        return "BGRA";
    return NULL;
}

// Load image file (PNG/JPEG) and call anpr_read_pixels
void readPixelBuffer(const char *imgfile, const char *outputFormat, const char *options)
{
    printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options);

    unsigned char *pixels = NULL;
    int width = 0, height = 0, channels = 0, stride = 0;
    const char *pixelFormat = NULL;
    int ok = 0;

    char ext[16] = {0};
    if (!get_lowercase_ext(imgfile, ext, sizeof(ext)))
    {
        printf("No file extension found!\n");
        return;
    }

    // Compare extension in lowercase
    if (strcmp(ext, "png") == 0)
    {
        stride = read_png(imgfile, &pixels, &width, &height, &channels);
        ok = stride > 0;
    }
    else if (strcmp(ext, "jpg") == 0 || strcmp(ext, "jpeg") == 0)
    {
        stride = read_jpeg(imgfile, &pixels, &width, &height, &channels);
        ok = stride > 0;
    }
    else
    {
        printf("Unsupported image format!\n");
        return;
    }

    if (!ok)
    {
        perror("Image load failed!");
        if (pixels)
            free(pixels);
        return;
    }

    pixelFormat = getPixelFormat(channels);
    if (!pixelFormat)
    {
        perror("Unknown pixel format!");
        free(pixels);
        return;
    }

    const char *result = tsanpr.anpr_read_pixels(
        pixels,
        width,
        height,
        stride,
        pixelFormat,
        outputFormat,
        options);
    printf("%s\n", result);
    free(pixels);
}

int readLicensePlates(const char *countryCode)
{
    // NOTICE:
    // anpr_initialize should be called only once after library load.
    // Therefore, it is not possible to change the country code after anpr_initialize has been called.
    // While using the free trial license, you can try all languages.
    // When you purchase a commercial license, you can only use the selected language.
    char initParams[128];
    snprintf(initParams, sizeof(initParams), "text;country=%s", countryCode);
    const char *error = tsanpr.anpr_initialize(initParams);
    if (error && error[0])
    {
        printf("anpr_initialize() failed (error=%s)\n", error);
        return -1;
    }

    char imageDir[512];
    snprintf(imageDir, sizeof(imageDir), "%s/img/%s/", examplesBaseDir, countryCode);

    // TODO: Try each function as needed
    void (*anprFunc)(const char *, const char *, const char *) = readImageFile;
    // void (*anprFunc)(const char *, const char *, const char *) = readEncodedImage;
    // void (*anprFunc)(const char *, const char *, const char *) = readPixelBuffer;

    // TODO: Try each output format as needed
    const char *outputFormat = "text";
    // const char* outputFormat = "json";
    // const char* outputFormat = "yaml";
    // const char* outputFormat = "xml";
    // const char* outputFormat = "csv";

    char path[1024];
    snprintf(path, sizeof(path), "%slicensePlate.jpg", imageDir);
    anprFunc(path, outputFormat, ""); // Single license plate recognition (default)

    snprintf(path, sizeof(path), "%smultiple.jpg", imageDir);
    anprFunc(path, outputFormat, "vm"); // Recognize multiple license plates attached to vehicles

    anprFunc(path, outputFormat, "vmb"); // Recognize multiple license plates including motorcycles

    snprintf(path, sizeof(path), "%ssurround.jpg", imageDir);
    anprFunc(path, outputFormat, "vms"); // Recognize multiple license plates with surround detection

    anprFunc(path, outputFormat, "dms"); // Recognize multiple surrounding objects (vehicles)

    anprFunc(path, outputFormat, "dmsr"); // Recognize multiple surrounding objects and license plates

    // Recognize multiple surrounding objects and license plates within RoI
    anprFunc(path, outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700");

    return 0;
}

int main(int ac, char **av)
{
    const char *engineFileName = getEngineFileName();
    int res = 0;
#ifdef _WIN32
    wchar_t *wEngineFileName = to_wstring(engineFileName);
    res = TSANPR_load(&tsanpr, wEngineFileName);
    free(wEngineFileName);
#else
    res = TSANPR_load(&tsanpr, engineFileName);
#endif
    if (res < 0)
        return res;

    // TODO: Try each country code as needed
    readLicensePlates("KR");
    // readLicensePlates("JP");
    // readLicensePlates("VN");

    TSANPR_unload();
    return 0;
}
