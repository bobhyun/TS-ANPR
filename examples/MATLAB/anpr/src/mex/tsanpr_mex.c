/*
 * The MIT License (MIT)
 * Copyright (c) 2022-2025 TS-Solution Corp.
 *
 * MEX wrapper for TSANPR library
 * Supports both MATLAB and GNU Octave
 *
 * Build:
 *   MATLAB: mex tsanpr_mex.c
 *   Octave: mkoctfile --mex tsanpr_mex.c
 */

#include "mex.h"
#include <string.h>
#include <stdlib.h>

#ifdef _WIN32
    #include <windows.h>
    #define LOAD_LIBRARY(path) LoadLibraryA(path)
    #define GET_PROC(lib, name) GetProcAddress((HMODULE)lib, name)
    #define FREE_LIBRARY(lib) FreeLibrary((HMODULE)lib)
    typedef HMODULE LibHandle;
#else
    #include <dlfcn.h>
    #define LOAD_LIBRARY(path) dlopen(path, RTLD_LAZY)
    #define GET_PROC(lib, name) dlsym(lib, name)
    #define FREE_LIBRARY(lib) dlclose(lib)
    typedef void* LibHandle;
#endif

/* Function pointer types */
typedef const char* (*anpr_initialize_func)(const char* mode);
typedef const char* (*anpr_read_file_func)(const char* img_file_name, const char* output_format, const char* options);
typedef const char* (*anpr_read_pixels_func)(const void* pixels, uint64_t width, uint64_t height, int64_t stride,
                                             const char* pixel_format, const char* output_format, const char* options);

/* Global state */
static LibHandle g_library = NULL;
static anpr_initialize_func g_anpr_initialize = NULL;
static anpr_read_file_func g_anpr_read_file = NULL;
static anpr_read_pixels_func g_anpr_read_pixels = NULL;

/* Helper: Get string from mxArray */
static char* get_string(const mxArray* arr) {
    if (!mxIsChar(arr)) {
        return NULL;
    }
    return mxArrayToString(arr);
}

/* Command: load_library */
static void cmd_load_library(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    char* lib_path;

    if (nrhs < 2) {
        mexErrMsgIdAndTxt("TSANPR:InvalidInput", "Library path required");
        return;
    }

    lib_path = get_string(prhs[1]);
    if (!lib_path) {
        mexErrMsgIdAndTxt("TSANPR:InvalidInput", "Library path must be a string");
        return;
    }

    /* Unload existing library if loaded */
    if (g_library) {
        FREE_LIBRARY(g_library);
        g_library = NULL;
        g_anpr_initialize = NULL;
        g_anpr_read_file = NULL;
        g_anpr_read_pixels = NULL;
    }

    /* Load the library */
    g_library = LOAD_LIBRARY(lib_path);
    mxFree(lib_path);

    if (!g_library) {
#ifdef _WIN32
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "Failed to load library (error code: %lu)", GetLastError());
        plhs[0] = mxCreateString(err_msg);
#else
        plhs[0] = mxCreateString(dlerror());
#endif
        return;
    }

    /* Get function pointers */
    g_anpr_initialize = (anpr_initialize_func)GET_PROC(g_library, "anpr_initialize");
    g_anpr_read_file = (anpr_read_file_func)GET_PROC(g_library, "anpr_read_file");
    g_anpr_read_pixels = (anpr_read_pixels_func)GET_PROC(g_library, "anpr_read_pixels");

    if (!g_anpr_initialize || !g_anpr_read_file || !g_anpr_read_pixels) {
        FREE_LIBRARY(g_library);
        g_library = NULL;
        plhs[0] = mxCreateString("Failed to get function pointers from library");
        return;
    }

    /* Success - return empty string */
    plhs[0] = mxCreateString("");
}

/* Command: unload_library */
static void cmd_unload_library(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (g_library) {
        FREE_LIBRARY(g_library);
        g_library = NULL;
        g_anpr_initialize = NULL;
        g_anpr_read_file = NULL;
        g_anpr_read_pixels = NULL;
    }
    plhs[0] = mxCreateString("");
}

/* Command: is_loaded */
static void cmd_is_loaded(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    plhs[0] = mxCreateLogicalScalar(g_library != NULL);
}

/* Command: initialize */
static void cmd_initialize(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    char* mode;
    const char* result;

    if (!g_library || !g_anpr_initialize) {
        plhs[0] = mxCreateString("Library not loaded");
        return;
    }

    if (nrhs < 2) {
        mexErrMsgIdAndTxt("TSANPR:InvalidInput", "Mode parameter required");
        return;
    }

    mode = get_string(prhs[1]);
    if (!mode) {
        mexErrMsgIdAndTxt("TSANPR:InvalidInput", "Mode must be a string");
        return;
    }

    result = g_anpr_initialize(mode);
    mxFree(mode);

    if (result && strlen(result) > 0) {
        plhs[0] = mxCreateString(result);
    } else {
        plhs[0] = mxCreateString("");
    }
}

/* Command: read_file */
static void cmd_read_file(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    char *img_file, *output_format, *options;
    const char* result;

    if (!g_library || !g_anpr_read_file) {
        plhs[0] = mxCreateString("Library not loaded");
        return;
    }

    if (nrhs < 4) {
        mexErrMsgIdAndTxt("TSANPR:InvalidInput", "Required: img_file, output_format, options");
        return;
    }

    img_file = get_string(prhs[1]);
    output_format = get_string(prhs[2]);
    options = get_string(prhs[3]);

    if (!img_file || !output_format || !options) {
        if (img_file) mxFree(img_file);
        if (output_format) mxFree(output_format);
        if (options) mxFree(options);
        mexErrMsgIdAndTxt("TSANPR:InvalidInput", "All parameters must be strings");
        return;
    }

    result = g_anpr_read_file(img_file, output_format, options);

    mxFree(img_file);
    mxFree(output_format);
    mxFree(options);

    if (result) {
        plhs[0] = mxCreateString(result);
    } else {
        plhs[0] = mxCreateString("");
    }
}

/* Command: read_pixels */
static void cmd_read_pixels(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    char *pixel_format, *output_format, *options;
    const char* result;
    uint8_t* pixels;
    uint64_t width, height;
    int64_t stride;
    size_t num_elements;

    if (!g_library || !g_anpr_read_pixels) {
        plhs[0] = mxCreateString("Library not loaded");
        return;
    }

    if (nrhs < 8) {
        mexErrMsgIdAndTxt("TSANPR:InvalidInput",
            "Required: pixels, width, height, stride, pixel_format, output_format, options");
        return;
    }

    /* Get pixel data */
    if (!mxIsUint8(prhs[1])) {
        mexErrMsgIdAndTxt("TSANPR:InvalidInput", "Pixels must be uint8 array");
        return;
    }
    pixels = (uint8_t*)mxGetData(prhs[1]);
    num_elements = mxGetNumberOfElements(prhs[1]);

    /* Get dimensions */
    width = (uint64_t)mxGetScalar(prhs[2]);
    height = (uint64_t)mxGetScalar(prhs[3]);
    stride = (int64_t)mxGetScalar(prhs[4]);

    /* Get string parameters */
    pixel_format = get_string(prhs[5]);
    output_format = get_string(prhs[6]);
    options = get_string(prhs[7]);

    if (!pixel_format || !output_format || !options) {
        if (pixel_format) mxFree(pixel_format);
        if (output_format) mxFree(output_format);
        if (options) mxFree(options);
        mexErrMsgIdAndTxt("TSANPR:InvalidInput", "Format and options must be strings");
        return;
    }

    result = g_anpr_read_pixels(pixels, width, height, stride, pixel_format, output_format, options);

    mxFree(pixel_format);
    mxFree(output_format);
    mxFree(options);

    if (result) {
        plhs[0] = mxCreateString(result);
    } else {
        plhs[0] = mxCreateString("");
    }
}

/* MEX entry point */
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    char* cmd;

    if (nrhs < 1 || !mxIsChar(prhs[0])) {
        mexErrMsgIdAndTxt("TSANPR:InvalidInput",
            "First argument must be command string: load_library, unload_library, is_loaded, initialize, read_file, read_pixels");
        return;
    }

    cmd = get_string(prhs[0]);
    if (!cmd) {
        mexErrMsgIdAndTxt("TSANPR:InvalidInput", "Failed to get command string");
        return;
    }

    if (strcmp(cmd, "load_library") == 0) {
        cmd_load_library(nlhs, plhs, nrhs, prhs);
    } else if (strcmp(cmd, "unload_library") == 0) {
        cmd_unload_library(nlhs, plhs, nrhs, prhs);
    } else if (strcmp(cmd, "is_loaded") == 0) {
        cmd_is_loaded(nlhs, plhs, nrhs, prhs);
    } else if (strcmp(cmd, "initialize") == 0) {
        cmd_initialize(nlhs, plhs, nrhs, prhs);
    } else if (strcmp(cmd, "read_file") == 0) {
        cmd_read_file(nlhs, plhs, nrhs, prhs);
    } else if (strcmp(cmd, "read_pixels") == 0) {
        cmd_read_pixels(nlhs, plhs, nrhs, prhs);
    } else {
        mxFree(cmd);
        mexErrMsgIdAndTxt("TSANPR:InvalidCommand", "Unknown command: %s", cmd);
        return;
    }

    mxFree(cmd);
}
