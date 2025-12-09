/*
The MIT License (MIT)
Copyright © 2022-2025 TS-Solution Corp.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to all conditions.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#include <erl_nif.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>

#ifdef _WIN32
    #include <windows.h>
    typedef HMODULE LibHandle;
    #define LOAD_LIB(path) LoadLibraryA(path)
    #define GET_FUNC(lib, name) GetProcAddress(lib, name)
    #define CLOSE_LIB(lib) FreeLibrary(lib)
#else
    #include <dlfcn.h>
    typedef void* LibHandle;
    #define LOAD_LIB(path) dlopen(path, RTLD_LAZY)
    #define GET_FUNC(lib, name) dlsym(lib, name)
    #define CLOSE_LIB(lib) dlclose(lib)
#endif

// Function pointer types for TSANPR API
typedef const char* (*anpr_initialize_func)(const char*);
typedef const char* (*anpr_read_file_func)(const char*, const char*, const char*);
typedef const char* (*anpr_read_pixels_func)(const void*, unsigned long, unsigned long, long, const char*, const char*, const char*);

// Global library handle and function pointers
static LibHandle g_lib_handle = NULL;
static anpr_initialize_func g_anpr_initialize = NULL;
static anpr_read_file_func g_anpr_read_file = NULL;
static anpr_read_pixels_func g_anpr_read_pixels = NULL;

// Resource type for library handle
static ErlNifResourceType* g_tsanpr_resource_type = NULL;

typedef struct {
    LibHandle lib_handle;
} TSANPRResource;

// Helper function to create Erlang string from C string
static ERL_NIF_TERM make_string_or_empty(ErlNifEnv* env, const char* str) {
    if (str == NULL || str[0] == '\0') {
        return enif_make_string(env, "", ERL_NIF_LATIN1);
    }
    return enif_make_string(env, str, ERL_NIF_LATIN1);
}

// NIF: load(LibraryPath) -> {ok, Resource} | {error, Reason}
static ERL_NIF_TERM load_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char lib_path[1024];

    if (!enif_get_string(env, argv[0], lib_path, sizeof(lib_path), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    // Load the library
    LibHandle lib = LOAD_LIB(lib_path);
    if (lib == NULL) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_string(env, "Failed to load library", ERL_NIF_LATIN1));
    }

    // Load function pointers
    g_anpr_initialize = (anpr_initialize_func)GET_FUNC(lib, "anpr_initialize");
    g_anpr_read_file = (anpr_read_file_func)GET_FUNC(lib, "anpr_read_file");
    g_anpr_read_pixels = (anpr_read_pixels_func)GET_FUNC(lib, "anpr_read_pixels");

    if (g_anpr_initialize == NULL || g_anpr_read_file == NULL || g_anpr_read_pixels == NULL) {
        CLOSE_LIB(lib);
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_string(env, "Failed to load TSANPR functions", ERL_NIF_LATIN1));
    }

    // Store library handle globally (simple approach for single library)
    g_lib_handle = lib;

    // Create resource to track library lifetime
    TSANPRResource* res = enif_alloc_resource(g_tsanpr_resource_type, sizeof(TSANPRResource));
    res->lib_handle = lib;
    ERL_NIF_TERM term = enif_make_resource(env, res);
    enif_release_resource(res);

    return enif_make_tuple2(env,
        enif_make_atom(env, "ok"),
        term);
}

// NIF: anpr_initialize(Resource, Mode) -> String
static ERL_NIF_TERM anpr_initialize_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char mode[1024];
    TSANPRResource* res;

    if (!enif_get_resource(env, argv[0], g_tsanpr_resource_type, (void**)&res)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_string(env, argv[1], mode, sizeof(mode), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    if (g_anpr_initialize == NULL) {
        return enif_make_string(env, "Library not loaded", ERL_NIF_LATIN1);
    }

    const char* result = g_anpr_initialize(mode);
    return make_string_or_empty(env, result);
}

// NIF: anpr_read_file(Resource, ImgFileName, OutputFormat, Options) -> String
static ERL_NIF_TERM anpr_read_file_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char img_file_name[1024];
    char output_format[64];
    char options[1024];
    TSANPRResource* res;

    if (!enif_get_resource(env, argv[0], g_tsanpr_resource_type, (void**)&res)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_string(env, argv[1], img_file_name, sizeof(img_file_name), ERL_NIF_LATIN1) ||
        !enif_get_string(env, argv[2], output_format, sizeof(output_format), ERL_NIF_LATIN1) ||
        !enif_get_string(env, argv[3], options, sizeof(options), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    if (g_anpr_read_file == NULL) {
        return enif_make_string(env, "Library not loaded", ERL_NIF_LATIN1);
    }

    const char* result = g_anpr_read_file(img_file_name, output_format, options);
    return make_string_or_empty(env, result);
}

// NIF: anpr_read_pixels(Resource, Pixels, Width, Height, Stride, PixelFormat, OutputFormat, Options) -> String
static ERL_NIF_TERM anpr_read_pixels_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    TSANPRResource* res;
    ErlNifBinary pixels;
    ErlNifUInt64 w64, h64;
    ErlNifSInt64 s64;
    unsigned long width, height;
    long stride;
    char pixel_format[64];
    char output_format[64];
    char options[1024];

    if (!enif_get_resource(env, argv[0], g_tsanpr_resource_type, (void**)&res)) {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[1], &pixels)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint64(env, argv[2], &w64) ||
        !enif_get_uint64(env, argv[3], &h64) ||
        !enif_get_int64(env, argv[4], &s64) ||
        !enif_get_string(env, argv[5], pixel_format, sizeof(pixel_format), ERL_NIF_LATIN1) ||
        !enif_get_string(env, argv[6], output_format, sizeof(output_format), ERL_NIF_LATIN1) ||
        !enif_get_string(env, argv[7], options, sizeof(options), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    // Validate and cast to API-expected widths (unsigned long / long)
    // Note: On Windows, unsigned long is 32-bit. Ensure values fit.
    if (w64 > (ErlNifUInt64)(~(unsigned long)0) ||
        h64 > (ErlNifUInt64)(~(unsigned long)0) ||
        s64 < (ErlNifSInt64)LONG_MIN || s64 > (ErlNifSInt64)LONG_MAX) {
        return enif_make_badarg(env);
    }
    width = (unsigned long)w64;
    height = (unsigned long)h64;
    stride = (long)s64;

    if (g_anpr_read_pixels == NULL) {
        return enif_make_string(env, "Library not loaded", ERL_NIF_LATIN1);
    }

    const char* result = g_anpr_read_pixels(pixels.data, width, height, stride,
                                           pixel_format, output_format, options);
    return make_string_or_empty(env, result);
}

// Resource destructor
static void tsanpr_resource_dtor(ErlNifEnv* env, void* obj) {
    TSANPRResource* res = (TSANPRResource*)obj;
    if (res->lib_handle != NULL && res->lib_handle == g_lib_handle) {
        CLOSE_LIB(res->lib_handle);
        g_lib_handle = NULL;
        g_anpr_initialize = NULL;
        g_anpr_read_file = NULL;
        g_anpr_read_pixels = NULL;
    }
}

// Module load callback
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    g_tsanpr_resource_type = enif_open_resource_type(env, NULL, "tsanpr_resource",
                                                      tsanpr_resource_dtor,
                                                      ERL_NIF_RT_CREATE, NULL);
    if (g_tsanpr_resource_type == NULL) {
        return -1;
    }
    return 0;
}

// NIF function array
static ErlNifFunc nif_funcs[] = {
    {"load_nif", 1, load_nif, 0},
    {"anpr_initialize_nif", 2, anpr_initialize_nif, 0},
    {"anpr_read_file_nif", 4, anpr_read_file_nif, 0},
    {"anpr_read_pixels_nif", 8, anpr_read_pixels_nif, 0}
};

ERL_NIF_INIT(tsanpr, nif_funcs, load, NULL, NULL, NULL)
