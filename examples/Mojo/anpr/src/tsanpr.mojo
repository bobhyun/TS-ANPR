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

from sys.ffi import DLHandle, c_char, c_size_t
from memory import UnsafePointer


fn _c_str_to_string(ptr: UnsafePointer[c_char]) -> String:
    """Convert a C string pointer to a Mojo String."""
    if not ptr:
        return ""
    return String(unsafe_from_utf8_ptr=ptr)


struct TSANPR:
    """
    TSANPR wrapper class for Mojo.
    Provides interface to the native TSANPR library.
    """
    var _lib: DLHandle

    fn __init__(out self, library_path: String) raises:
        """
        Initialize TSANPR with the given library path.
        """
        self._lib = DLHandle(library_path)

    fn anpr_initialize(self, owned mode: String) -> String:
        """
        Initialize the ANPR engine with the specified mode.

        Args:
            mode: Initialization mode string (e.g., "text;country=KR")

        Returns:
            Error message if initialization failed, empty string if successful.
        """
        var func = self._lib.get_function[
            fn(UnsafePointer[c_char]) -> UnsafePointer[c_char]
        ]("anpr_initialize")
        var result_ptr = func(mode.unsafe_cstr_ptr())
        return _c_str_to_string(result_ptr)

    fn anpr_read_file(self, owned img_file_name: String, owned output_format: String, owned options: String) -> String:
        """
        Read and process an image file.

        Args:
            img_file_name: Path to the image file.
            output_format: Output format (text, json, yaml, xml, csv).
            options: Processing options.

        Returns:
            Recognition result as string.
        """
        var func = self._lib.get_function[
            fn(UnsafePointer[c_char], UnsafePointer[c_char], UnsafePointer[c_char]) -> UnsafePointer[c_char]
        ]("anpr_read_file")
        var result_ptr = func(
            img_file_name.unsafe_cstr_ptr(),
            output_format.unsafe_cstr_ptr(),
            options.unsafe_cstr_ptr()
        )
        return _c_str_to_string(result_ptr)

    fn anpr_read_pixels(
        self,
        pixels: UnsafePointer[UInt8],
        width: Int,
        height: Int,
        stride: Int,
        owned pixel_format: String,
        owned output_format: String,
        owned options: String
    ) -> String:
        """
        Process pixel data directly.

        Args:
            pixels: Pointer to pixel data.
            width: Image width (or encoded data length for 'encoded' format).
            height: Image height (0 for 'encoded' format).
            stride: Row stride in bytes (0 for 'encoded' format).
            pixel_format: Pixel format (BGR, BGRA, GRAY, encoded, etc.).
            output_format: Output format (text, json, yaml, xml, csv).
            options: Processing options.

        Returns:
            Recognition result as string.
        """
        var func = self._lib.get_function[
            fn(UnsafePointer[UInt8], c_size_t, c_size_t, Int64,
               UnsafePointer[c_char], UnsafePointer[c_char], UnsafePointer[c_char]) -> UnsafePointer[c_char]
        ]("anpr_read_pixels")
        var result_ptr = func(
            pixels,
            c_size_t(width),
            c_size_t(height),
            Int64(stride),
            pixel_format.unsafe_cstr_ptr(),
            output_format.unsafe_cstr_ptr(),
            options.unsafe_cstr_ptr()
        )
        return _c_str_to_string(result_ptr)
