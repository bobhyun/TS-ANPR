//
// The MIT License (MIT)
// Copyright © 2022-2025 TS-Solution Corp.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to all conditions.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//

import Foundation

#if os(Windows)
import WinSDK
#endif

typealias AnprInitializeFunc = @convention(c) (UnsafePointer<CChar>?) -> UnsafePointer<CChar>?
typealias AnprReadFileFunc = @convention(c) (UnsafePointer<CChar>?, UnsafePointer<CChar>?, UnsafePointer<CChar>?) -> UnsafePointer<CChar>?
typealias AnprReadPixelsFunc = @convention(c) (
    UnsafePointer<UInt8>?, UInt, UInt, Int64,
    UnsafePointer<CChar>?, UnsafePointer<CChar>?, UnsafePointer<CChar>?
) -> UnsafePointer<CChar>?

/// TSANPR dynamic loader and function pointer holder
class TSANPR {
    var handle: UnsafeMutableRawPointer?
    var anpr_initialize: AnprInitializeFunc?
    var anpr_read_file: AnprReadFileFunc?
    var anpr_read_pixels: AnprReadPixelsFunc?

    func load(libraryPath: String) -> Int {
        unload()
        #if os(Windows)
        let wpath = Array(libraryPath.utf16) + [0]
        handle = UnsafeMutableRawPointer(LoadLibraryW(wpath))
        guard let handle = handle else {
            print("Cannot load module: \(libraryPath)")
            return -2
        }
        anpr_initialize = unsafeBitCast(GetProcAddress(HMODULE(bitPattern: UInt(bitPattern: handle)), "anpr_initialize"), to: AnprInitializeFunc?.self)
        if anpr_initialize == nil {
            print("anpr_initialize() not found.")
            unload()
            return -3
        }
        anpr_read_file = unsafeBitCast(GetProcAddress(HMODULE(bitPattern: UInt(bitPattern: handle)), "anpr_read_file"), to: AnprReadFileFunc?.self)
        if anpr_read_file == nil {
            print("anpr_read_file() not found.")
            unload()
            return -4
        }
        anpr_read_pixels = unsafeBitCast(GetProcAddress(HMODULE(bitPattern: UInt(bitPattern: handle)), "anpr_read_pixels"), to: AnprReadPixelsFunc?.self)
        if anpr_read_pixels == nil {
            print("anpr_read_pixels() not found.")
            unload()
            return -5
        }
        #else
        handle = dlopen(libraryPath, RTLD_LAZY)
        guard let handle = handle else {
            perror("Cannot load module")
            return -2
        }
        anpr_initialize = unsafeBitCast(dlsym(handle, "anpr_initialize"), to: AnprInitializeFunc?.self)
        if anpr_initialize == nil {
            print("anpr_initialize() not found.")
            unload()
            return -3
        }
        anpr_read_file = unsafeBitCast(dlsym(handle, "anpr_read_file"), to: AnprReadFileFunc?.self)
        if anpr_read_file == nil {
            print("anpr_read_file() not found.")
            unload()
            return -4
        }
        anpr_read_pixels = unsafeBitCast(dlsym(handle, "anpr_read_pixels"), to: AnprReadPixelsFunc?.self)
        if anpr_read_pixels == nil {
            print("anpr_read_pixels() not found.")
            unload()
            return -5
        }
        #endif
        return 0
    }

    func unload() {
        #if os(Windows)
        if let handle = handle {
            FreeLibrary(HMODULE(bitPattern: UInt(bitPattern: handle)))
        }
        #else
        if let handle = handle {
            dlclose(handle)
        }
        #endif
        handle = nil
        anpr_initialize = nil
        anpr_read_file = nil
        anpr_read_pixels = nil
    }
}
