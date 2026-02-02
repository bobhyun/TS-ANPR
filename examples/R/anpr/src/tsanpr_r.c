// R wrapper for TSANPR library
// The MIT License (MIT)
// Copyright © 2022-2025 TS-Solution Corp.

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
typedef HMODULE lib_handle_t;
#define LOAD_LIBRARY(path) LoadLibraryA(path)
#define GET_SYMBOL(handle, name) GetProcAddress(handle, name)
#define CLOSE_LIBRARY(handle) FreeLibrary(handle)
#else
#include <dlfcn.h>
typedef void* lib_handle_t;
#define LOAD_LIBRARY(path) dlopen(path, RTLD_LAZY)
#define GET_SYMBOL(handle, name) dlsym(handle, name)
#define CLOSE_LIBRARY(handle) dlclose(handle)
#endif

// Function pointer types matching TSANPR API
typedef const char* (*anpr_initialize_func)(const char* mode);
typedef const char* (*anpr_read_file_func)(const char* img_file, const char* output_format, const char* options);
typedef const char* (*anpr_read_pixels_func)(const unsigned char* pixels, unsigned long long width,
                                              unsigned long long height, long long stride,
                                              const char* pixel_format, const char* output_format,
                                              const char* options);

// Global library handle and function pointers
static lib_handle_t g_lib_handle = NULL;
static anpr_initialize_func g_anpr_initialize = NULL;
static anpr_read_file_func g_anpr_read_file = NULL;
static anpr_read_pixels_func g_anpr_read_pixels = NULL;

// Load the TSANPR library
SEXP tsanpr_load(SEXP library_path) {
    if (g_lib_handle != NULL) {
        return ScalarInteger(0);  // Already loaded
    }

    const char* path = CHAR(STRING_ELT(library_path, 0));
    g_lib_handle = LOAD_LIBRARY(path);

    if (g_lib_handle == NULL) {
        return ScalarInteger(-1);  // Failed to load
    }

    // Load function pointers
    g_anpr_initialize = (anpr_initialize_func)GET_SYMBOL(g_lib_handle, "anpr_initialize");
    g_anpr_read_file = (anpr_read_file_func)GET_SYMBOL(g_lib_handle, "anpr_read_file");
    g_anpr_read_pixels = (anpr_read_pixels_func)GET_SYMBOL(g_lib_handle, "anpr_read_pixels");

    if (g_anpr_initialize == NULL || g_anpr_read_file == NULL || g_anpr_read_pixels == NULL) {
        CLOSE_LIBRARY(g_lib_handle);
        g_lib_handle = NULL;
        return ScalarInteger(-2);  // Symbol not found
    }

    return ScalarInteger(0);  // Success
}

// Unload the TSANPR library
SEXP tsanpr_unload(void) {
    if (g_lib_handle != NULL) {
        CLOSE_LIBRARY(g_lib_handle);
        g_lib_handle = NULL;
        g_anpr_initialize = NULL;
        g_anpr_read_file = NULL;
        g_anpr_read_pixels = NULL;
    }
    return R_NilValue;
}

// Check if library is loaded
SEXP tsanpr_is_loaded(void) {
    return ScalarLogical(g_lib_handle != NULL);
}

// Initialize the ANPR engine
SEXP tsanpr_initialize(SEXP mode) {
    if (g_anpr_initialize == NULL) {
        return mkString("Error: Library not loaded");
    }

    const char* mode_str = CHAR(STRING_ELT(mode, 0));
    const char* result = g_anpr_initialize(mode_str);

    if (result == NULL) {
        return mkString("");
    }
    return mkString(result);
}

// Read and process an image file
SEXP tsanpr_read_file(SEXP img_file, SEXP output_format, SEXP options) {
    if (g_anpr_read_file == NULL) {
        return mkString("Error: Library not loaded");
    }

    const char* img_str = CHAR(STRING_ELT(img_file, 0));
    const char* fmt_str = CHAR(STRING_ELT(output_format, 0));
    const char* opt_str = CHAR(STRING_ELT(options, 0));

    const char* result = g_anpr_read_file(img_str, fmt_str, opt_str);

    if (result == NULL) {
        return mkString("");
    }
    return mkString(result);
}

// Process pixel data
SEXP tsanpr_read_pixels(SEXP pixels, SEXP width, SEXP height, SEXP stride,
                        SEXP pixel_format, SEXP output_format, SEXP options) {
    if (g_anpr_read_pixels == NULL) {
        return mkString("Error: Library not loaded");
    }

    const unsigned char* pixel_data = RAW(pixels);
    unsigned long long w = (unsigned long long)asInteger(width);
    unsigned long long h = (unsigned long long)asInteger(height);
    long long s = (long long)asInteger(stride);
    const char* pix_fmt = CHAR(STRING_ELT(pixel_format, 0));
    const char* out_fmt = CHAR(STRING_ELT(output_format, 0));
    const char* opt_str = CHAR(STRING_ELT(options, 0));

    const char* result = g_anpr_read_pixels(pixel_data, w, h, s, pix_fmt, out_fmt, opt_str);

    if (result == NULL) {
        return mkString("");
    }
    return mkString(result);
}

// R function registration
static const R_CallMethodDef CallEntries[] = {
    {"tsanpr_load", (DL_FUNC) &tsanpr_load, 1},
    {"tsanpr_unload", (DL_FUNC) &tsanpr_unload, 0},
    {"tsanpr_is_loaded", (DL_FUNC) &tsanpr_is_loaded, 0},
    {"tsanpr_initialize", (DL_FUNC) &tsanpr_initialize, 1},
    {"tsanpr_read_file", (DL_FUNC) &tsanpr_read_file, 3},
    {"tsanpr_read_pixels", (DL_FUNC) &tsanpr_read_pixels, 7},
    {NULL, NULL, 0}
};

void R_init_tsanpr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
