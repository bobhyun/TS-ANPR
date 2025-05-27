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
 */

import 'dart:ffi';
import 'dart:io';
import 'package:ffi/ffi.dart';

/// FFI signatures for TSANPR shared library functions.
typedef anpr_initialize_native = Pointer<Utf8> Function(Pointer<Utf8> mode);
typedef anpr_read_file_native = Pointer<Utf8> Function(
  Pointer<Utf8> imgFileName,
  Pointer<Utf8> outputFormat,
  Pointer<Utf8> options,
);
typedef anpr_read_pixels_native = Pointer<Utf8> Function(
  Pointer<Uint8> pixels,
  Uint64 width,
  Uint64 height,
  Int64 stride,
  Pointer<Utf8> pixelFormat,
  Pointer<Utf8> outputFormat,
  Pointer<Utf8> options,
);

class TSANPR {
  late final DynamicLibrary _lib;
  late final Pointer<Utf8> Function(Pointer<Utf8> mode) anpr_initialize;
  late final Pointer<Utf8> Function(
    Pointer<Utf8> imgFileName,
    Pointer<Utf8> outputFormat,
    Pointer<Utf8> options,
  ) anpr_read_file;
  late final Pointer<Utf8> Function(
    Pointer<Uint8> pixels,
    int width,
    int height,
    int stride,
    Pointer<Utf8> pixelFormat,
    Pointer<Utf8> outputFormat,
    Pointer<Utf8> options,
  ) anpr_read_pixels;

  /// Loads the TSANPR shared library.
  TSANPR(String engineFileName) {
    _lib = DynamicLibrary.open(engineFileName);

    // Load function pointers.
    anpr_initialize = _lib
        .lookup<NativeFunction<anpr_initialize_native>>('anpr_initialize')
        .asFunction();
    anpr_read_file = _lib
        .lookup<NativeFunction<anpr_read_file_native>>('anpr_read_file')
        .asFunction();
    anpr_read_pixels = _lib
        .lookup<NativeFunction<anpr_read_pixels_native>>('anpr_read_pixels')
        .asFunction();
  }

  /// Unload is not required in Dart, as the library is unloaded when the process ends.
}
