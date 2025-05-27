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
import time
from tsanpr.tsanpr import TSANPR

BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))

def get_engine_file_name():
    arch = platform.machine()
    if sys.platform.startswith("win"):
        if arch in ("AMD64", "x86_64"):
            return os.path.join(BASE_DIR, "bin/windows-x86_64/tsanpr.dll")
        elif arch in ("x86", "i386"):
            return os.path.join(BASE_DIR, "bin/windows-x86/tsanpr.dll")
    elif sys.platform.startswith("linux"):
        if arch in ("x86_64", "amd64"):
            return os.path.join(BASE_DIR, "bin/linux-x86_64/libtsanpr.so")
        elif arch == "aarch64":
            return os.path.join(BASE_DIR, "bin/linux-aarch64/libtsanpr.so")
    return ""

def webcam_read_license_plates(tsanpr, country_code):
    error = tsanpr.anpr_initialize(f"text;country={country_code}")
    if error:
        print(f"anpr_initialize() failed: {error}")
        return
    
    output_format = "text"

    capture = cv2.VideoCapture(0)
    capture.set(cv2.CAP_PROP_FRAME_WIDTH, 640)
    capture.set(cv2.CAP_PROP_FRAME_HEIGHT, 480)
    fps = capture.get(cv2.CAP_PROP_FPS)
    if fps == 0:
        fps = 30  # default value (depend on your cam)
    delay_default = 1000 / fps

    delay = delay_default
    while True:
        start = time.time()
        ret, frame = capture.read()
        if not ret:
            print("Camera read failed")
            break

        height, width = frame.shape[:2]
        stride = frame.strides[0]
        # frame.tobytes() returns a contiguous BGR byte array.
        result = tsanpr.anpr_read_pixels(
            frame.tobytes(), width, height, stride, "BGR", output_format, ""
        )
        if result:
            print(result)

        cv2.imshow('Webcam Demo - Please show the vehicle to the camera.', frame)
        spent = (time.time() - start) * 1000  # millie second unit
        key = cv2.waitKey(max(1, int(delay - spent)))
        if key == 27:  # Exit with the ESC key.
            break

    capture.release()
    cv2.destroyAllWindows()

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
    webcam_read_license_plates(tsanpr, "KR")
    # webcam_read_license_plates(tsanpr, "JP")
    # webcam_read_license_plates(tsanpr, "VN")

if __name__ == "__main__":
    main()
