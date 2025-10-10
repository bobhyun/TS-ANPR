/*
 * The MIT License (MIT)
 * Copyright © 2022-2025 TS-Solution Corp.
 *
 * COBOL-friendly wrapper for TS-ANPR engine (Version 2 - fixed COBOL string handling)
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef _WIN32
    #include <windows.h>
    #define LIBRARY_HANDLE HMODULE
    #define LOAD_LIBRARY(path) LoadLibraryA(path)
    #define GET_FUNCTION(lib, name) GetProcAddress(lib, name)
    #define FREE_LIBRARY(lib) FreeLibrary(lib)
    #define EXPORT __declspec(dllexport)
#else
    #include <dlfcn.h>
    #define LIBRARY_HANDLE void*
    #define LOAD_LIBRARY(path) dlopen(path, RTLD_LAZY)
    #define GET_FUNCTION(lib, name) dlsym(lib, name)
    #define FREE_LIBRARY(lib) dlclose(lib)
    #define EXPORT
#endif

/* Function pointer types */
typedef const char* (*anpr_initialize_func)(const char*);
typedef const char* (*anpr_read_file_func)(const char*, const char*, const char*);

/* Global state */
static LIBRARY_HANDLE g_library = NULL;
static anpr_initialize_func g_anpr_initialize = NULL;
static anpr_read_file_func g_anpr_read_file = NULL;

/* Get default library path based on platform */
static const char* get_default_library_path(void) {
#ifdef _WIN32
    #ifdef _WIN64
        return "../../bin/windows-x86_64/tsanpr.dll";
    #else
        return "../../bin/windows-x86/tsanpr.dll";
    #endif
#else
    #ifdef __aarch64__
        return "../../bin/linux-aarch64/libtsanpr.so";
    #else
        return "../../bin/linux-x86_64/libtsanpr.so";
    #endif
#endif
}

/* Load TS-ANPR library and get function pointers */
static int load_tsanpr_library(const char* lib_path) {
    if (g_library != NULL) {
        return 0; /* Already loaded */
    }

    g_library = LOAD_LIBRARY(lib_path);
    if (g_library == NULL) {
        return -1;
    }

    g_anpr_initialize = (anpr_initialize_func)GET_FUNCTION(g_library, "anpr_initialize");
    g_anpr_read_file = (anpr_read_file_func)GET_FUNCTION(g_library, "anpr_read_file");

    if (g_anpr_initialize == NULL || g_anpr_read_file == NULL) {
        FREE_LIBRARY(g_library);
        g_library = NULL;
        return -2;
    }

    return 0;
}

/* Helper to create null-terminated string from COBOL string (may have trailing spaces) */
static char* cobol_to_c_string(const char* cobol_str, int max_len) {
    int len = 0;
    int i;

    /* Find actual length (excluding trailing spaces) */
    for (i = 0; i < max_len && cobol_str[i] != '\0'; i++) {
        if (cobol_str[i] != ' ') {
            len = i + 1;
        }
    }

    char* c_str = (char*)malloc(len + 1);
    if (c_str) {
        memcpy(c_str, cobol_str, len);
        c_str[len] = '\0';
    }
    return c_str;
}

/* Helper to copy C string to COBOL string (null-terminated, no padding) */
static void c_to_cobol_string(const char* c_str, char* cobol_str, int cobol_len) {
    int len = c_str ? strlen(c_str) : 0;
    int i;

    if (len >= cobol_len) {
        len = cobol_len - 1;  /* Leave room for null terminator */
    }

    for (i = 0; i < len; i++) {
        cobol_str[i] = c_str[i];
    }

    /* Null terminate (COBOL can handle null-terminated strings) */
    cobol_str[i] = '\0';
}

/* Initialize ANPR engine with options */
EXPORT int tsanpr_cobol_initialize(char* options, int options_len,
                                    char* result, int result_len) {
    char* c_options = NULL;
    const char* lib_path;
    int load_result;
    const char* init_result;

    /* Load library if not already loaded */
    if (g_library == NULL) {
        lib_path = get_default_library_path();
        load_result = load_tsanpr_library(lib_path);
        if (load_result != 0) {
            c_to_cobol_string("ERROR: Failed to load TS-ANPR library", result, result_len);
            return load_result;
        }
    }

    /* Convert COBOL string to C string */
    c_options = cobol_to_c_string(options, options_len);
    if (c_options == NULL) {
        c_to_cobol_string("ERROR: Memory allocation failed", result, result_len);
        return -4;
    }

    /* Call anpr_initialize */
    init_result = g_anpr_initialize(c_options);
    free(c_options);

    if (init_result == NULL || strlen(init_result) == 0) {
        c_to_cobol_string("OK", result, result_len);
        return 0;
    } else {
        c_to_cobol_string(init_result, result, result_len);
        return -3;
    }
}

/* Read image file and perform ANPR */
EXPORT int tsanpr_cobol_read_file(char* image_path, int image_path_len,
                                   char* output_format, int output_format_len,
                                   char* options, int options_len,
                                   char* result, int result_len) {
    char* c_image_path = NULL;
    char* c_output_format = NULL;
    char* c_options = NULL;
    const char* anpr_result;
    int ret = 0;

    if (g_library == NULL || g_anpr_read_file == NULL) {
        c_to_cobol_string("ERROR: Library not initialized", result, result_len);
        return -1;
    }

    /* Convert COBOL strings to C strings */
    c_image_path = cobol_to_c_string(image_path, image_path_len);
    c_output_format = cobol_to_c_string(output_format, output_format_len);
    c_options = cobol_to_c_string(options, options_len);

    if (!c_image_path || !c_output_format || !c_options) {
        c_to_cobol_string("ERROR: Memory allocation failed", result, result_len);
        ret = -4;
        goto cleanup;
    }

    /* Call anpr_read_file */
    anpr_result = g_anpr_read_file(c_image_path, c_output_format, c_options);
    c_to_cobol_string(anpr_result, result, result_len);

cleanup:
    if (c_image_path) free(c_image_path);
    if (c_output_format) free(c_output_format);
    if (c_options) free(c_options);

    return ret;
}

/* Cleanup resources */
EXPORT void tsanpr_cobol_cleanup(void) {
    if (g_library != NULL) {
        FREE_LIBRARY(g_library);
        g_library = NULL;
        g_anpr_initialize = NULL;
        g_anpr_read_file = NULL;
    }
}