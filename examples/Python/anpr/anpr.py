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

import sys
import platform
import os
import cv2
import numpy as np
import ctypes
from tsanpr.tsanpr import TSANPR

EXAMPLES_BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))

def get_engine_file_name():
    arch = platform.machine()
    if sys.platform.startswith("win"):
        if arch in ("AMD64", "x86_64"):
            return os.path.join(EXAMPLES_BASE_DIR, "bin/windows-x86_64/tsanpr.dll")
        elif arch in ("x86", "i386"):
            return os.path.join(EXAMPLES_BASE_DIR, "bin/windows-x86/tsanpr.dll")
    elif sys.platform.startswith("linux"):
        if arch in ("x86_64", "amd64"):
            return os.path.join(EXAMPLES_BASE_DIR, "bin/linux-x86_64/libtsanpr.so")
        elif arch == "aarch64":
            return os.path.join(EXAMPLES_BASE_DIR, "bin/linux-aarch64/libtsanpr.so")
    return ""

def read_image_file(tsanpr, imgfile, output_format, options):
    """
    Read an image file and call anpr_read_file.
    """
    print(f'{imgfile} (outputFormat="{output_format}", options="{options}") => ', end="")
    result = tsanpr.anpr_read_file(imgfile, output_format, options)
    print(result)

def read_encoded_image(tsanpr, imgfile, output_format, options):
    """
    Read an encoded image file as bytes and call tsanpr.anpr_read_pixels with 'encoded' pixel format.
    """
    print(f'{imgfile} (outputFormat="{output_format}", options="{options}") => ', end="")
    try:
        with open(imgfile, "rb") as f:
            encoded_img = f.read()
        # Create a ctypes buffer from the bytes
        buf = (ctypes.c_ubyte * len(encoded_img)).from_buffer_copy(encoded_img)
        ptr = ctypes.cast(buf, ctypes.c_void_p)
        result = tsanpr.anpr_read_pixels(
            ptr,
            len(encoded_img),
            0,
            0,
            "encoded",
            output_format,
            options
        )
        print(result)
    except Exception as ex:
        print(f"\nERROR: Exception - {ex}")

def get_pixel_format(img):
    """
    Determine pixel format string based on OpenCV image channels.
    """
    channels = 1 if len(img.shape) == 2 else img.shape[2]
    if channels == 1:
        return "GRAY"
    elif channels == 2:
        return "BGR565"  # or "BGR555"
    elif channels == 3:
        return "BGR"
    elif channels == 4:
        return "BGRA"
    else:
        return None

def read_pixel_buffer(tsanpr, imgfile, output_format, options):
    """
    Use the pixel buffer-based ANPR function.
    """
    print(f'{imgfile} (outputFormat="{output_format}", options="{options}") => ', end="")
    img = cv2.imread(imgfile, cv2.IMREAD_UNCHANGED)
    if img is None or img.size == 0:
        print("Image load failed!")
        return

    pixel_format = get_pixel_format(img)
    if pixel_format is None:
        print("Unknown pixel format!")
        return

    height, width = img.shape[:2]
    stride = img.strides[0]
    img_data = img.ctypes.data_as(ctypes.c_void_p)
    result = tsanpr.anpr_read_pixels(
        img_data,
        width,
        height,
        stride,
        pixel_format,
        output_format,
        options
    )
    print(result)

def read_license_plates(tsanpr, country_code):
    """
    NOTICE:
    anpr_initialize should be called only once after library load.
    Therefore, it is not possible to change the country code after anpr_initialize has been called.
    While using the free trial license, you can try all languages.
    When you purchase a commercial license, you can only use the selected language.
    """
    error = tsanpr.anpr_initialize(f"text;country={country_code}")
    if error:
        print(f"anpr_initialize() failed: {error}")
        return
        
    image_dir = os.path.join(EXAMPLES_BASE_DIR, "img", country_code)

    # TODO: Try each function as needed
    anpr_func = read_image_file
    # anpr_func = read_encoded_image
    # anpr_func = read_pixel_buffer

    # TODO: Try each output format as needed
    output_format = "text"
    # output_format = "json"
    # output_format = "yaml"
    # output_format = "xml"
    # output_format = "csv"

    anpr_func(tsanpr, os.path.join(image_dir, "licensePlate.jpg"), output_format, "")   # Single license plate recognition (default)
    anpr_func(tsanpr, os.path.join(image_dir, "multiple.jpg"), output_format, "vm")     # Recognize multiple license plates attached to vehicles
    anpr_func(tsanpr, os.path.join(image_dir, "multiple.jpg"), output_format, "vmb")    # Recognize multiple license plates attached to vehicles (including motorcycles)
    anpr_func(tsanpr, os.path.join(image_dir, "surround.jpg"), output_format, "vms")    # Recognize multiple license plates attached to vehicles with surround detection
    anpr_func(tsanpr, os.path.join(image_dir, "surround.jpg"), output_format, "dms")    # Recognize multiple surrounding objects (vehicles)
    anpr_func(tsanpr, os.path.join(image_dir, "surround.jpg"), output_format, "dmsr")   # Recognize multiple surrounding objects (vehicles) and license plates

    # Recognize multiple surrounding objects and license plates within RoI
    anpr_func(tsanpr, os.path.join(image_dir, "surround.jpg"), output_format, "dmsri549,700,549,2427,1289,2427,1289,700")
    
def main():
    engine_file_name = get_engine_file_name()
    if not engine_file_name or not os.path.exists(engine_file_name):
        print("Unsupported operating system or engine not found")
        return
    try:
        tsanpr = TSANPR(engine_file_name)
    except Exception as ex:
        print(f"TSANPR initialization failed: {ex}")
        return
    
    # TODO: Try each country code as needed
    read_license_plates(tsanpr, "KR")
    # read_license_plates(tsanpr, "JP")
    # read_license_plates(tsanpr, "VN")

if __name__ == "__main__":
    main()
