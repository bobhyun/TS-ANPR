# =============================================================================
# The MIT License (MIT)
# Copyright © 2022-2025 TS-Solution Corp.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to all conditions.
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
# =============================================================================

include("lib/tsanpr.jl")
using .TSANPR

const examplesBaseDir = joinpath("..", "..")

# Generate engine filename depending on platform
function getEngineFileName()
    if Sys.iswindows()
        engine_path = joinpath(examplesBaseDir, "bin", "windows-x86_64", "tsanpr.dll")
    elseif Sys.islinux()
        engine_path = joinpath(examplesBaseDir, "bin", "linux-x86_64", "libtsanpr.so")
    else
        println("Unsupported OS: $(Sys.KERNEL)")
        exit(1)
    end
    return engine_path
end

# Read image file and recognize license plate
function readImageFile(imgfile::String, outputFormat::String, options::String)
    println("$imgfile (outputFormat=\"$outputFormat\", options=\"$options\") => ")
    result = TSANPR.anpr_read_file(imgfile, outputFormat, options)
    println(result)
end

# Read encoded image file (raw bytes, e.g. JPEG/PNG)
function readEncodedImage(imgfile::String, outputFormat::String, options::String)
    println("$imgfile (outputFormat=\"$outputFormat\", options=\"$options\") => ")

    try
        encodedImg = read(imgfile)
        result = TSANPR.anpr_read_pixels(encodedImg, UInt32(length(encodedImg)), UInt32(0), Int32(0),
                                         "encoded", outputFormat, options)
        println(result)
    catch
        println("File open failed")
        return
    end
end

# Read pixel buffer (for example, from OpenCV or other raw image loader)
# This is a stub; in real use, you would use a Julia image library to load the image and get pixel buffer, width, height, stride, pixelFormat
function readPixelBuffer(imgfile::String, outputFormat::String, options::String)
    println("$imgfile (outputFormat=\"$outputFormat\", options=\"$options\") => ")
    println("readPixelBuffer: Not implemented in pure Julia. Use Images.jl or OpenCV.jl to load image and extract pixel buffer.")
end

# Recognize license plates for a given country code
function readLicensePlates(countryCode::String)
    # NOTICE:
    # anpr_initialize should be called only once after library load.
    # Therefore, it is not possible to change the country code after anpr_initialize has been called.
    # While using the free trial license, you can try all languages.
    # When you purchase a commercial license, you can only use the selected language.
    initParams = "text;country=" * countryCode
    TSANPR.anpr_initialize(initParams)

    imageDir = joinpath(examplesBaseDir, "img", countryCode)

    # TODO: Try each function as needed
    anprFunc = readImageFile
    # anprFunc = readEncodedImage
    # anprFunc = readPixelBuffer

    # TODO: Try each output format as needed
    outputFormat = "text"
    # outputFormat = "json"
    # outputFormat = "yaml"
    # outputFormat = "xml"
    # outputFormat = "csv"

    anprFunc(joinpath(imageDir, "licensePlate.jpg"), outputFormat, "") # Single license plate recognition (default)
    anprFunc(joinpath(imageDir, "multiple.jpg"), outputFormat, "vm")   # Recognize multiple license plates attached to vehicles
    anprFunc(joinpath(imageDir, "multiple.jpg"), outputFormat, "vmb")  # Recognize multiple license plates including motorcycles
    anprFunc(joinpath(imageDir, "surround.jpg"), outputFormat, "vms")  # Recognize multiple license plates with surround detection
    anprFunc(joinpath(imageDir, "surround.jpg"), outputFormat, "dms")  # Recognize multiple surrounding objects (vehicles)
    anprFunc(joinpath(imageDir, "surround.jpg"), outputFormat, "dmsr") # Recognize multiple surrounding objects and license plates
    # Recognize multiple surrounding objects and license plates within RoI
    anprFunc(joinpath(imageDir, "surround.jpg"), outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700")
end

# Main entry point
function main()
    engineFileName = getEngineFileName()
    if !TSANPR.load_tsanpr(engineFileName)
        println("TSANPR engine library load failed. Exiting program.")
        return
    end

    # TODO: Try each country code as needed
    readLicensePlates("KR")
    # readLicensePlates("JP")
    # readLicensePlates("VN")
end

main()
