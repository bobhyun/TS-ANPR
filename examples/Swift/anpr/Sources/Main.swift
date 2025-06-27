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
import CBridge

// C struct import
struct ImageBuffer {
    var data: UnsafeMutablePointer<UInt8>?
    var width: UInt
    var height: UInt
    var stride: Int64
    var pixelFormat: UnsafePointer<CChar>?
}

let examplesBaseDir = "../.."

// Generate engine filename depending on platform
func getEngineFileName() -> String {
#if os(Windows)
    #if arch(x86_64)
    // 64-bit Windows
    return "\(examplesBaseDir)\\bin\\windows-x86_64\\tsanpr.dll"
    #else
    // 32-bit Windows
    return "\(examplesBaseDir)\\bin\\windows-x86\\tsanpr.dll"
    #endif
#elseif arch(x86_64)
    // 64-bit Linux
    return "\(examplesBaseDir)/bin/linux-x86_64/libtsanpr.so"
#elseif arch(arm64)
    // 64-bit ARM Linux
    return "\(examplesBaseDir)/bin/linux-aarch64/libtsanpr.so"
#else
    return ""
#endif
}

/// Recognize license plate from image file.
func readImageFile(tsanpr: TSANPR, imgfile: String, outputFormat: String, options: String) {
    print("\(imgfile) (outputFormat=\"\(outputFormat)\", options=\"\(options)\") => ", terminator: "")
    let resultPtr = tsanpr.anpr_read_file!(imgfile, outputFormat, options)
    let result = resultPtr.map { String(cString: $0) } ?? "nil"
    print(result)
}

/// Recognize license plate from encoded image buffer.
func readEncodedImage(tsanpr: TSANPR, imgfile: String, outputFormat: String, options: String) {
    print("\(imgfile) (outputFormat=\"\(outputFormat)\", options=\"\(options)\") => ", terminator: "")
    guard let data = try? Data(contentsOf: URL(fileURLWithPath: imgfile)) else {
        print("File open failed")
        return
    }

    let pixelPtr: UnsafePointer<UInt8>? = data.withUnsafeBytes { rawBuf in
        return rawBuf.baseAddress?.assumingMemoryBound(to: UInt8.self)
    }
    let resultPtr = tsanpr.anpr_read_pixels!(
        pixelPtr,
        UInt(data.count),
        0,
        0,
        "encoded",
        outputFormat,
        options
    )
    let result = resultPtr.map { String(cString: $0) } ?? "nil"
    print(result)
}

// Recognize license plate from pixel buffer (supports PNG/JPEG only)
func readPixelBuffer(tsanpr: TSANPR, imgfile: String, outputFormat: String, options: String) {
    print("\(imgfile) (outputFormat=\"\(outputFormat)\", options=\"\(options)\") => ", terminator: "")
    imgfile.withCString { cstr in
        let buf = load_image(cstr)
        guard let dataPtr = buf.data, let pixelFormatPtr = buf.pixelFormat else {
            print("Image load failed or unknown pixel format!")
            return
        }
        let pixelFormat = String(cString: pixelFormatPtr)
        let resultPtr = tsanpr.anpr_read_pixels!(
            dataPtr,
            UInt(buf.width),
            UInt(buf.height),
            Int64(buf.stride),
            pixelFormat,
            outputFormat,
            options
        )
        let result = resultPtr.map { String(cString: $0) } ?? "nil"
        print(result)
        free_image_buffer(buf)
    }
}

/// Recognize license plates for a specific country code.
@discardableResult
func readLicensePlates(tsanpr: TSANPR, countryCode: String) -> Int {
    // NOTICE:
    // anpr_initialize should be called only once after library load.
    // Therefore, it is not possible to change the country code after anpr_initialize has been called.
    // While using the free trial license, you can try all languages.
    // When you purchase a commercial license, you can only use the selected language.
    let initParams = "text;country=\(countryCode)"
    guard let fn = tsanpr.anpr_initialize else {
        print("anpr_initialize function pointer not loaded.")
        return -1
    }
    let errorPtr = fn(initParams)
    if let errorPtr, errorPtr.pointee != 0 {
        let error = String(cString: errorPtr)
        print("anpr_initialize() failed (error=\(error))")
        return -1
    }

    let imageDir = "\(examplesBaseDir)/img/\(countryCode)/"

    // TODO: Try each function as needed
    let anprFunc: (TSANPR, String, String, String) -> Void = readImageFile
    // let anprFunc: (TSANPR, String, String, String) -> Void = readEncodedImage
    // let anprFunc: (TSANPR, String, String, String) -> Void = readPixelBuffer

    // TODO: Try each output format as needed
    let outputFormat = "text"
    // let outputFormat = "json"
    // let outputFormat = "yaml"
    // let outputFormat = "xml"
    // let outputFormat = "csv"

    anprFunc(tsanpr, imageDir + "licensePlate.jpg", outputFormat, "") // Single license plate recognition (default)
    anprFunc(tsanpr, imageDir + "multiple.jpg", outputFormat, "vm") // Recognize multiple license plates attached to vehicles
    anprFunc(tsanpr, imageDir + "multiple.jpg", outputFormat, "vmb") // Recognize multiple license plates including motorcycles
    anprFunc(tsanpr, imageDir + "surround.jpg", outputFormat, "vms") // Recognize multiple license plates with surround detection
    anprFunc(tsanpr, imageDir + "surround.jpg", outputFormat, "dms") // Recognize multiple surrounding objects (vehicles)
    anprFunc(tsanpr, imageDir + "surround.jpg", outputFormat, "dmsr") // Recognize multiple surrounding objects and license plates
    
    // Recognize multiple surrounding objects and license plates within RoI
    anprFunc(tsanpr, imageDir + "surround.jpg", outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700")
    
    return 0
}

@main
struct App {
    static func main() {
        let engineFileName = getEngineFileName()
        let tsanpr = TSANPR()
        let res = tsanpr.load(libraryPath: engineFileName)
        if res < 0 {
            exit(Int32(res))
        }

        // TODO: Try each country code as needed
        _ = readLicensePlates(tsanpr: tsanpr, countryCode: "KR")
        // _ = readLicensePlates(tsanpr: tsanpr, countryCode: "JP")
        // _ = readLicensePlates(tsanpr: tsanpr, countryCode: "VN")
        tsanpr.unload()
    }
}
