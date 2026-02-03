/*
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
 */

import com.sun.jna.Library
import com.sun.jna.Native
import com.sun.jna.Pointer

/**
 * TSANPR JNA Interface
 * Defines the native library functions for ANPR engine.
 */
interface TSANPRLibrary extends Library {
    String anpr_initialize(String mode)
    String anpr_read_file(String imgFileName, String outputFormat, String options)
    String anpr_read_pixels(Pointer pixels, long width, long height, long stride,
                           String pixelFormat, String outputFormat, String options)
}

/**
 * TSANPR Wrapper Class
 * Provides convenient methods for license plate recognition.
 */
class TSANPR {
    private TSANPRLibrary lib

    TSANPR(String libraryPath) {
        if (!new File(libraryPath).exists()) {
            throw new RuntimeException("Library file not found: ${libraryPath}")
        }
        lib = Native.load(libraryPath, TSANPRLibrary.class) as TSANPRLibrary
    }

    String initialize(String mode) {
        return lib.anpr_initialize(mode) ?: ""
    }

    String readFile(String imgFileName, String outputFormat, String options) {
        return lib.anpr_read_file(imgFileName, outputFormat, options) ?: ""
    }

    String readPixels(Pointer pixels, long width, long height, long stride,
                     String pixelFormat, String outputFormat, String options) {
        return lib.anpr_read_pixels(pixels, width, height, stride,
                                    pixelFormat, outputFormat, options) ?: ""
    }
}
