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

// tsanpr_windows.go
package tsanpr

/*
#include <windows.h>

typedef char* (__cdecl *anpr_initialize_func)(const char*);
typedef char* (__cdecl *anpr_read_file_func)(const char*, const char*, const char*);
typedef char* (__cdecl *anpr_read_pixels_func)(const void*, unsigned long long, unsigned long long, long long, const char*, const char*, const char*);
*/
import "C"
import (
	"syscall"
	"unsafe"
)

type TSANPR struct {
	libHandle       syscall.Handle
	anprInitialize  uintptr
	anprReadFile    uintptr
	anprReadPixels  uintptr
}

func LoadLibrary(path string) (*TSANPR, error) {
	lib, err := syscall.LoadLibrary(path)
	if err != nil {
		return nil, err
	}
	getProc := func(name string) (uintptr, error) {
		return syscall.GetProcAddress(lib, name)
	}
	initPtr, err := getProc("anpr_initialize")
	if err != nil {
		syscall.FreeLibrary(lib)
		return nil, err
	}
	readFilePtr, err := getProc("anpr_read_file")
	if err != nil {
		syscall.FreeLibrary(lib)
		return nil, err
	}
	readPixelsPtr, err := getProc("anpr_read_pixels")
	if err != nil {
		syscall.FreeLibrary(lib)
		return nil, err
	}
	return &TSANPR{
		libHandle:      lib,
		anprInitialize: initPtr,
		anprReadFile:   readFilePtr,
		anprReadPixels: readPixelsPtr,
	}, nil
}

func (t *TSANPR) Dispose() {
	if t.libHandle != 0 {
		syscall.FreeLibrary(t.libHandle)
		t.libHandle = 0
	}
}

func (t *TSANPR) AnprInitialize(mode string) string {
	cmode := syscall.StringBytePtr(mode)
	ret, _, _ := syscall.Syscall(t.anprInitialize, 1, uintptr(unsafe.Pointer(cmode)), 0, 0)
	if ret == 0 {
		return ""
	}
	return C.GoString((*C.char)(unsafe.Pointer(ret)))
}

func (t *TSANPR) AnprReadFile(imgFileName, outputFormat, options string) string {
	cimg := syscall.StringBytePtr(imgFileName)
	cout := syscall.StringBytePtr(outputFormat)
	copt := syscall.StringBytePtr(options)
	ret, _, _ := syscall.Syscall(t.anprReadFile, 3,
		uintptr(unsafe.Pointer(cimg)),
		uintptr(unsafe.Pointer(cout)),
		uintptr(unsafe.Pointer(copt)))
	if ret == 0 {
		return ""
	}
	return C.GoString((*C.char)(unsafe.Pointer(ret)))
}

func (t *TSANPR) AnprReadPixels(pixels unsafe.Pointer, width, height uint64, stride int64, pixelFormat, outputFormat, options string) string {
	cpixfmt := syscall.StringBytePtr(pixelFormat)
	cout := syscall.StringBytePtr(outputFormat)
	copt := syscall.StringBytePtr(options)
	ret, _, _ := syscall.Syscall9(t.anprReadPixels, 7,
		uintptr(pixels),
		uintptr(width),
		uintptr(height),
		uintptr(stride),
		uintptr(unsafe.Pointer(cpixfmt)),
		uintptr(unsafe.Pointer(cout)),
		uintptr(unsafe.Pointer(copt)),
		0, 0)
	if ret == 0 {
		return ""
	}
	return C.GoString((*C.char)(unsafe.Pointer(ret)))
}
