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

// tsanpr_linux.go
package tsanpr

/*
#cgo LDFLAGS: -ldl
#include <stdlib.h>
#include <stdint.h>
#include <dlfcn.h>

// define if RTLD_LAZY is not defined
#ifndef RTLD_LAZY
#define RTLD_LAZY 0x00001
#endif

typedef char* (*anpr_initialize_func)(const char*);
typedef char* (*anpr_read_file_func)(const char*, const char*, const char*);
typedef char* (*anpr_read_pixels_func)(const void*, uint64_t, uint64_t, int64_t, const char*, const char*, const char*);

// dlopen/dlsym/dlclose wrapper
static void* go_dlopen(const char* filename, int flag) {
    return dlopen(filename, flag);
}
static void* go_dlsym(void* handle, const char* symbol) {
    return dlsym(handle, symbol);
}
static int go_dlclose(void* handle) {
    return dlclose(handle);
}

// function pointer wrapper
static char* call_anpr_initialize(void* f, const char* mode) {
    return ((anpr_initialize_func)f)(mode);
}
static char* call_anpr_read_file(void* f, const char* img, const char* out, const char* opt) {
    return ((anpr_read_file_func)f)(img, out, opt);
}
static char* call_anpr_read_pixels(void* f, const void* pix, uint64_t w, uint64_t h, int64_t s, const char* pf, const char* of, const char* op) {
    return ((anpr_read_pixels_func)f)(pix, w, h, s, pf, of, op);
}
*/
import "C"
import (
	"errors"
	"unsafe"
)

type TSANPR struct {
	libHandle      unsafe.Pointer
	anprInitialize unsafe.Pointer
	anprReadFile   unsafe.Pointer
	anprReadPixels unsafe.Pointer
}

func LoadLibrary(path string) (*TSANPR, error) {
	cpath := C.CString(path)
	defer C.free(unsafe.Pointer(cpath))

	handle := C.go_dlopen(cpath, C.int(C.RTLD_LAZY))
	if handle == nil {
		return nil, errors.New("failed to load native library: " + path)
	}

	getSym := func(name string) unsafe.Pointer {
		cname := C.CString(name)
		defer C.free(unsafe.Pointer(cname))
		return C.go_dlsym(handle, cname)
	}

	initSym := getSym("anpr_initialize")
	readFileSym := getSym("anpr_read_file")
	readPixelsSym := getSym("anpr_read_pixels")

	if initSym == nil || readFileSym == nil || readPixelsSym == nil {
		C.go_dlclose(handle)
		return nil, errors.New("failed to bind native functions")
	}

	return &TSANPR{
		libHandle:      handle,
		anprInitialize: initSym,
		anprReadFile:   readFileSym,
		anprReadPixels: readPixelsSym,
	}, nil
}

func (t *TSANPR) Dispose() {
	if t.libHandle != nil {
		C.go_dlclose(t.libHandle)
		t.libHandle = nil
	}
}

func (t *TSANPR) AnprInitialize(mode string) string {
	cmode := C.CString(mode)
	defer C.free(unsafe.Pointer(cmode))
	result := C.call_anpr_initialize(t.anprInitialize, cmode)
	if result == nil {
		return ""
	}
	return C.GoString(result)
}

func (t *TSANPR) AnprReadFile(imgFileName, outputFormat, options string) string {
	cimg := C.CString(imgFileName)
	cout := C.CString(outputFormat)
	copt := C.CString(options)
	defer C.free(unsafe.Pointer(cimg))
	defer C.free(unsafe.Pointer(cout))
	defer C.free(unsafe.Pointer(copt))
	result := C.call_anpr_read_file(t.anprReadFile, cimg, cout, copt)
	if result == nil {
		return ""
	}
	return C.GoString(result)
}

func (t *TSANPR) AnprReadPixels(pixels unsafe.Pointer, width, height uint64, stride int64, pixelFormat, outputFormat, options string) string {
	cpixfmt := C.CString(pixelFormat)
	cout := C.CString(outputFormat)
	copt := C.CString(options)
	defer C.free(unsafe.Pointer(cpixfmt))
	defer C.free(unsafe.Pointer(cout))
	defer C.free(unsafe.Pointer(copt))
	result := C.call_anpr_read_pixels(
		t.anprReadPixels,
		pixels,
		C.uint64_t(width), C.uint64_t(height), C.int64_t(stride),
		cpixfmt, cout, copt)
	if result == nil {
		return ""
	}
	return C.GoString(result)
}
