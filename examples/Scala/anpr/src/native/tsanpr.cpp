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

#ifdef _WIN32
#define _CRT_SECURE_NO_WARNINGS
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <wchar.h>
#else
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>
#include <errno.h>
#endif

#include "tsanpr.h"

#ifdef _WIN32
static HMODULE hlib = NULL;
#else
static void *hlib = NULL;
#endif

void TSANPR_unload(void)
{
#ifdef _WIN32
    if (hlib)
    {
        FreeLibrary(hlib);
        hlib = NULL;
    }
#else
    if (hlib)
    {
        dlclose(hlib);
        hlib = NULL;
    }
#endif
}

#ifdef _WIN32
int TSANPR_load(TSANPR *tsanpr, const wchar_t *engineFileName)
#else
int TSANPR_load(TSANPR *tsanpr, const char *engineFileName)
#endif
{
    if (!tsanpr || !engineFileName)
    {
        perror("invalid arguments.");
        return -1;
    }

    TSANPR_unload();

#ifdef _WIN32
    hlib = LoadLibraryW(engineFileName);
    if (!hlib)
    {
        perror("Cannot load module.");
        DWORD err = GetLastError();
        if (err)
        {
            LPSTR buf = NULL;
            FormatMessageA(
                FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL, err, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                (LPSTR)&buf, 0, NULL);
            if (buf)
            {
                perror(buf);
                LocalFree(buf);
            }
        }
        return -2;
    }
#else
    hlib = dlopen(engineFileName, RTLD_LAZY);
    if (!hlib)
    {
        perror("Cannot load module");
        return -2;
    }
#endif

    memset(tsanpr, 0, sizeof(TSANPR));

#ifdef _WIN32
    tsanpr->anpr_initialize = (const char *(WINAPI *)(const char *))GetProcAddress(hlib, "anpr_initialize");
#else
    tsanpr->anpr_initialize = (const char *(*)(const char *))dlsym(hlib, "anpr_initialize");
#endif
    if (!tsanpr->anpr_initialize)
    {
        perror("anpr_initialize() not found.");
        TSANPR_unload();
        return -3;
    }

#ifdef _WIN32
    tsanpr->anpr_read_file = (const char *(WINAPI *)(const char *, const char *, const char *))GetProcAddress(hlib, "anpr_read_file");
#else
    tsanpr->anpr_read_file = (const char *(*)(const char *, const char *, const char *))dlsym(hlib, "anpr_read_file");
#endif
    if (!tsanpr->anpr_read_file)
    {
        perror("anpr_read_file() not found.");
        TSANPR_unload();
        return -4;
    }

#ifdef _WIN32
    tsanpr->anpr_read_pixels = (const char *(WINAPI *)(const unsigned char *, const unsigned long, const unsigned long, const long, const char *, const char *, const char *))GetProcAddress(hlib, "anpr_read_pixels");
#else
    tsanpr->anpr_read_pixels = (const char *(*)(const unsigned char *, const unsigned long, const unsigned long, const long, const char *, const char *, const char *))dlsym(hlib, "anpr_read_pixels");
#endif
    if (!tsanpr->anpr_read_pixels)
    {
        perror("anpr_read_pixels() not found.");
        TSANPR_unload();
        return -5;
    }

    return 0;
}
