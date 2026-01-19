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

from sys.info import is_triple
from pathlib import Path
from memory import UnsafePointer
from tsanpr import TSANPR


fn get_engine_path() -> Path:
    """
    Get the engine file path depending on platform and architecture.
    """
    # Path relative to project directory (examples/Mojo/anpr)
    var base = Path("../../bin")

    @parameter
    if is_triple["x86_64-unknown-linux-gnu"]():
        return base / "linux-x86_64" / "libtsanpr.so"
    elif is_triple["aarch64-unknown-linux-gnu"]():
        return base / "linux-aarch64" / "libtsanpr.so"
    else:
        # Other platforms not currently supported
        return Path("")


fn process_webcam_frame(
    tsanpr: TSANPR,
    frame_data: UnsafePointer[UInt8],
    width: Int,
    height: Int,
    channels: Int
) -> String:
    """
    Process a single webcam frame for license plate recognition.

    Args:
        tsanpr: TSANPR instance.
        frame_data: Pointer to frame pixel data.
        width: Frame width.
        height: Frame height.
        channels: Number of color channels.

    Returns:
        Recognition result as string.
    """
    var pixel_format: String
    if channels == 1:
        pixel_format = "GRAY"
    elif channels == 3:
        pixel_format = "BGR"
    elif channels == 4:
        pixel_format = "BGRA"
    else:
        return "Unsupported pixel format"

    var stride = width * channels
    var result = tsanpr.anpr_read_pixels(
        frame_data,
        width,
        height,
        stride,
        pixel_format,
        "text",  # Output format
        "vm"     # Options: recognize multiple license plates attached to vehicles
    )

    return result


fn main():
    """
    Main webcam processing function.
    Note: This is a simplified example. In a real implementation,
    you would need to integrate with a webcam library or OpenCV equivalent.
    """
    var engine_path = get_engine_path()
    var engine_path_str = String(engine_path)
    if not engine_path_str:
        print("Unsupported operating system")
        return

    if not engine_path.exists():
        print("Engine not found:", engine_path_str)
        return

    try:
        var tsanpr = TSANPR(engine_path_str)

        # Initialize ANPR engine
        var error = tsanpr.anpr_initialize("text;country=KR")
        if error:
            print("anpr_initialize() failed:", error)
            return

        print("TSANPR webcam example initialized successfully")
        print("Note: Webcam integration requires additional implementation")
        print("This example shows the structure for processing webcam frames")

        # In a real implementation, you would:
        # 1. Initialize webcam/camera device
        # 2. Capture frames in a loop
        # 3. Process each frame with process_webcam_frame()
        # 4. Display results and/or save to file

        print("Webcam processing would start here...")

    except e:
        print("TSANPR initialization failed:", e)
