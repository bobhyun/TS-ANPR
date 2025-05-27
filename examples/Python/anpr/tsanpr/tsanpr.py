"""
The MIT License (MIT)
Copyright © 2022-2025 TS-Solution Corp.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to all conditions.

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
"""

import ctypes
import os

class TSANPR:
    def __init__(self, library_path):
        try:
            self.lib = ctypes.CDLL(library_path)
        except Exception as ex:
            raise RuntimeError(f"Failed to load native library: {library_path}\n{ex}")

        # 함수 시그니처 정의
        self.lib.anpr_initialize.restype = ctypes.c_void_p
        self.lib.anpr_initialize.argtypes = [ctypes.c_char_p]

        self.lib.anpr_read_file.restype = ctypes.c_void_p
        self.lib.anpr_read_file.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]

        self.lib.anpr_read_pixels.restype = ctypes.c_void_p
        self.lib.anpr_read_pixels.argtypes = [
            ctypes.c_void_p, ctypes.c_uint64, ctypes.c_uint64, ctypes.c_int64,
            ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p
        ]

    def anpr_initialize(self, mode: str) -> str:
        mode_bytes = mode.encode('utf-8')
        ptr = self.lib.anpr_initialize(mode_bytes)
        if not ptr:
            return ""
        result = ctypes.cast(ptr, ctypes.c_char_p).value
        if not result:
            return ""
        return result.decode('utf-8')

    def anpr_read_file(self, img_file_name: str, output_format: str, options: str) -> str:
        img_bytes = img_file_name.encode('utf-8')
        out_bytes = output_format.encode('utf-8')
        opt_bytes = options.encode('utf-8')
        ptr = self.lib.anpr_read_file(img_bytes, out_bytes, opt_bytes)
        if not ptr:
            return ""
        result = ctypes.cast(ptr, ctypes.c_char_p).value
        if not result:
            return ""
        return result.decode('utf-8')

    def anpr_read_pixels(self, pixels, width, height, stride, pixel_format, output_format, options) -> str:
        pf_bytes = pixel_format.encode('utf-8')
        out_bytes = output_format.encode('utf-8')
        opt_bytes = options.encode('utf-8')
        ptr = self.lib.anpr_read_pixels(
            pixels, ctypes.c_uint64(width), ctypes.c_uint64(height), ctypes.c_int64(stride),
            pf_bytes, out_bytes, opt_bytes
        )
        if not ptr:
            return ""
        result = ctypes.cast(ptr, ctypes.c_char_p).value
        if not result:
            return ""
        return result.decode('utf-8')
