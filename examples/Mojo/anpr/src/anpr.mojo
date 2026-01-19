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


fn read_image_file(tsanpr: TSANPR, imgfile: String, output_format: String, options: String):
    """
    Read an image file and call anpr_read_file.
    """
    print(imgfile, '(outputFormat="' + output_format + '", options="' + options + '") => ', end="")
    var result = tsanpr.anpr_read_file(imgfile, output_format, options)
    print(result)


fn read_encoded_image(tsanpr: TSANPR, imgfile: String, output_format: String, options: String) raises:
    """
    Read an encoded image file as bytes and call tsanpr.anpr_read_pixels with 'encoded' pixel format.
    """
    print(imgfile, '(outputFormat="' + output_format + '", options="' + options + '") => ', end="")
    var file_path = Path(imgfile)
    if not file_path.exists():
        print("\nERROR: File does not exist")
        return

    var encoded_img = file_path.read_bytes()
    var result = tsanpr.anpr_read_pixels(
        encoded_img.unsafe_ptr(),
        len(encoded_img),
        0,
        0,
        "encoded",
        output_format,
        options
    )
    print(result)


fn get_pixel_format(channels: Int) -> String:
    """
    Determine pixel format string based on image channels.
    """
    if channels == 1:
        return "GRAY"
    elif channels == 2:
        return "BGR565"  # or "BGR555"
    elif channels == 3:
        return "BGR"
    elif channels == 4:
        return "BGRA"
    else:
        return ""


fn read_pixel_buffer(tsanpr: TSANPR, imgfile: String, output_format: String, options: String) raises:
    """
    Use the pixel buffer-based ANPR function with encoded image format.

    Note: Since Mojo doesn't have a native image decoding library,
    this function reads the encoded image file and passes it to
    anpr_read_pixels with 'encoded' pixel format, which allows the
    engine to decode the image internally.

    For raw pixel buffer processing (BGR, GRAY, etc.), you would need
    to integrate with an image processing library like OpenCV via FFI.
    """
    print(imgfile, '(outputFormat="' + output_format + '", options="' + options + '") => ', end="")
    var file_path = Path(imgfile)
    if not file_path.exists():
        print("\nERROR: File does not exist")
        return

    var encoded_img = file_path.read_bytes()
    var result = tsanpr.anpr_read_pixels(
        encoded_img.unsafe_ptr(),
        len(encoded_img),  # width = encoded data length
        0,                 # height = 0 for encoded format
        0,                 # stride = 0 for encoded format
        "encoded",         # pixel format
        output_format,
        options
    )
    print(result)


fn read_license_plates(tsanpr: TSANPR, country_code: String) raises:
    """
    NOTICE:
    anpr_initialize should be called only once after library load.
    Therefore, it is not possible to change the country code after anpr_initialize has been called.
    While using the free trial license, you can try all languages.
    When you purchase a commercial license, you can only use the selected language.
    """
    var init_params = "text;country=" + country_code
    var error = tsanpr.anpr_initialize(init_params)
    if error:
        print("anpr_initialize() failed:", error)
        return

    # Path relative to project directory (examples/Mojo/anpr)
    var image_dir = Path("../../img") / country_code

    # TODO: Try each function as needed
    alias anpr_func = read_image_file
    # alias anpr_func = read_encoded_image
    # alias anpr_func = read_pixel_buffer

    # TODO: Try each output format as needed
    alias output_format = "text"
    # alias output_format = "json"
    # alias output_format = "yaml"
    # alias output_format = "xml"
    # alias output_format = "csv"

    anpr_func(tsanpr, String(image_dir / "licensePlate.jpg"), output_format, "")   # Single license plate recognition (default)
    anpr_func(tsanpr, String(image_dir / "multiple.jpg"), output_format, "vm")     # Recognize multiple license plates attached to vehicles
    anpr_func(tsanpr, String(image_dir / "multiple.jpg"), output_format, "vmb")    # Recognize multiple license plates attached to vehicles (including motorcycles)
    anpr_func(tsanpr, String(image_dir / "surround.jpg"), output_format, "vms")    # Recognize multiple license plates attached to vehicles with surround detection
    anpr_func(tsanpr, String(image_dir / "surround.jpg"), output_format, "dms")    # Recognize multiple surrounding objects (vehicles)
    anpr_func(tsanpr, String(image_dir / "surround.jpg"), output_format, "dmsr")   # Recognize multiple surrounding objects (vehicles) and license plates

    # Recognize multiple surrounding objects and license plates within RoI
    anpr_func(tsanpr, String(image_dir / "surround.jpg"), output_format, "dmsri549,700,549,2427,1289,2427,1289,700")


fn main():
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

        # TODO: Try each country code as needed
        read_license_plates(tsanpr, "KR")
        # read_license_plates(tsanpr, "JP")
        # read_license_plates(tsanpr, "VN")

    except e:
        print("TSANPR initialization failed:", e)
