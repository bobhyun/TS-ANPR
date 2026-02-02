// The MIT License (MIT)
// Copyright © 2022-2025 TS-Solution Corp.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to all conditions.

#include "tsanpr_wrapper.h"
#include <stddef.h>

#ifdef _WIN32
#include <windows.h>
typedef HMODULE LibHandle;
#define LOAD_LIBRARY(path) LoadLibraryA(path)
#define GET_SYMBOL(handle, name) GetProcAddress(handle, name)
#define CLOSE_LIBRARY(handle) FreeLibrary(handle)
#else
#include <dlfcn.h>
typedef void* LibHandle;
#define LOAD_LIBRARY(path) dlopen(path, RTLD_LAZY)
#define GET_SYMBOL(handle, name) dlsym(handle, name)
#define CLOSE_LIBRARY(handle) dlclose(handle)
#endif

// Function pointer types
typedef const char* (*AnprInitializeFunc)(const char*);
typedef const char* (*AnprReadFileFunc)(const char*, const char*, const char*);
typedef const char* (*AnprReadPixelsFunc)(const unsigned char*, unsigned long long,
                                          unsigned long long, long long,
                                          const char*, const char*, const char*);

// Library handle and function pointers
static LibHandle lib_handle = NULL;
static AnprInitializeFunc fn_initialize = NULL;
static AnprReadFileFunc fn_read_file = NULL;
static AnprReadPixelsFunc fn_read_pixels = NULL;

int tsanpr_load(const char* library_path) {
    if (lib_handle != NULL) {
        return 0; // Already loaded
    }

    lib_handle = LOAD_LIBRARY(library_path);
    if (lib_handle == NULL) {
        return -1;
    }

    fn_initialize = (AnprInitializeFunc)GET_SYMBOL(lib_handle, "anpr_initialize");
    fn_read_file = (AnprReadFileFunc)GET_SYMBOL(lib_handle, "anpr_read_file");
    fn_read_pixels = (AnprReadPixelsFunc)GET_SYMBOL(lib_handle, "anpr_read_pixels");

    if (fn_initialize == NULL || fn_read_file == NULL || fn_read_pixels == NULL) {
        CLOSE_LIBRARY(lib_handle);
        lib_handle = NULL;
        fn_initialize = NULL;
        fn_read_file = NULL;
        fn_read_pixels = NULL;
        return -2;
    }

    return 0;
}

void tsanpr_unload(void) {
    if (lib_handle != NULL) {
        CLOSE_LIBRARY(lib_handle);
        lib_handle = NULL;
        fn_initialize = NULL;
        fn_read_file = NULL;
        fn_read_pixels = NULL;
    }
}

int tsanpr_is_loaded(void) {
    return lib_handle != NULL ? 1 : 0;
}

const char* tsanpr_initialize(const char* mode) {
    if (fn_initialize == NULL) {
        return NULL;
    }
    return fn_initialize(mode);
}

const char* tsanpr_read_file(const char* img_file, const char* output_format, const char* options) {
    if (fn_read_file == NULL) {
        return NULL;
    }
    return fn_read_file(img_file, output_format, options);
}

const char* tsanpr_read_pixels(const unsigned char* pixels, unsigned long long width,
                               unsigned long long height, long long stride,
                               const char* pixel_format, const char* output_format,
                               const char* options) {
    if (fn_read_pixels == NULL) {
        return NULL;
    }
    return fn_read_pixels(pixels, width, height, stride, pixel_format, output_format, options);
}
