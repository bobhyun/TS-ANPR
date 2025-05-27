(*
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
*)

module Program

open System
open System.IO
open System.Runtime.InteropServices
open OpenCvSharp
open TSANPR

let examplesBaseDir = "../../../../../.."

let getEngineFileName () =
    let arch = RuntimeInformation.ProcessArchitecture
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        match arch with
        | Architecture.X86 -> examplesBaseDir + "/bin/windows-x86/tsanpr.dll"
        | Architecture.X64 -> examplesBaseDir + "/bin/windows-x86_64/tsanpr.dll"
        | _ -> ""
    elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
        match arch with
        | Architecture.X64 -> examplesBaseDir + "/bin/linux-x86_64/libtsanpr.so"
        | Architecture.Arm64 -> examplesBaseDir + "/bin/linux-aarch64/libtsanpr.so"
        | _ -> ""
    else
        ""

let readImageFile (tsanpr: TSANPR) (imgfile: string) (outputFormat: string) (options: string) =
    printf $"{imgfile} (outputFormat=\"{outputFormat}\", options=\"{options}\") => "
    let result = tsanpr.anpr_read_file(imgfile, outputFormat, options)
    printfn "%s" result

let readEncodedImage (tsanpr: TSANPR) (imgfile: string) (outputFormat: string) (options: string) =
    printf $"{imgfile} (outputFormat=\"{outputFormat}\", options=\"{options}\") => "
    try
        let encodedImg = File.ReadAllBytes(imgfile)
        let handle = GCHandle.Alloc(encodedImg, GCHandleType.Pinned)
        try
            let ptr = handle.AddrOfPinnedObject()
            let result = tsanpr.anpr_read_pixels(ptr, uint64 encodedImg.Length, 0UL, 0L, "encoded", outputFormat, options)
            printfn "%s" result
        finally
            handle.Free()
    with ex ->
        eprintfn "\nERROR: Exception - %s" ex.Message

let getPixelFormat (img: Mat) =
    match img.Channels() with
    | 1 -> "GRAY"
    | 2 -> "BGR565"
    | 3 -> "BGR"
    | 4 -> "BGRA"
    | _ -> null

let readPixelBuffer (tsanpr: TSANPR) (imgfile: string) (outputFormat: string) (options: string) =
    printf $"{imgfile} (outputFormat=\"{outputFormat}\", options=\"{options}\") => "
    use img = Cv2.ImRead(imgfile, ImreadModes.Unchanged)
    if img.Empty() then
        eprintfn "Image load failed!"
    else
        let pixelFormat = getPixelFormat img
        if isNull pixelFormat then
            eprintfn "Unknown pixel format!"
        else
            let imgData = img.Data
            let stride = int (img.Step())
            let result = tsanpr.anpr_read_pixels(imgData, uint64 img.Width, uint64 img.Height, int64 stride, pixelFormat, outputFormat, options)
            printfn "%s" result

let readLicensePlates (tsanpr: TSANPR) (countryCode: string) =

    // NOTICE:
    // anpr_initialize should be called only once after library load.
    // Therefore, it is not possible to change the country code after anpr_initialize has been called.
    // While using the free trial license, you can try all languages.
    // When you purchase a commercial license, you can only use the selected language.    
    let error = tsanpr.anpr_initialize($"text;country={countryCode}")

    if not (String.IsNullOrEmpty(error)) then
        printfn $"anpr_initialize() failed: {error}"
    else
        let imageDir = examplesBaseDir + $"/img/{countryCode}/"

        // TODO: Try each function as needed
        let anprFunc = readImageFile
        // let anprFunc = readEncodedImage
        // let anprFunc = readPixelBuffer

        // TODO: Try each output format as needed
        let outputFormat = "text"
        // let outputFormat = "json"
        // let outputFormat = "yaml"
        // let outputFormat = "xml"
        // let outputFormat = "csv"

        anprFunc tsanpr (imageDir + "licensePlate.jpg") outputFormat "" // Single license plate recognition (default)
        anprFunc tsanpr (imageDir + "multiple.jpg") outputFormat "vm"   // Recognize multiple license plates attached to vehicles
        anprFunc tsanpr (imageDir + "multiple.jpg") outputFormat "vmb"  // Recognize multiple license plates attached to vehicles (including motorcycles)
        anprFunc tsanpr (imageDir + "surround.jpg") outputFormat "vms"  // Recognize multiple license plates attached to vehicles with surround detection
        anprFunc tsanpr (imageDir + "surround.jpg") outputFormat "dms"  // Recognize multiple surrounding objects (vehicles)
        anprFunc tsanpr (imageDir + "surround.jpg") outputFormat "dmsr" // Recognize multiple surrounding objects (vehicles) and license plates

        // Recognize multiple surrounding objects and license plates within RoI
        anprFunc tsanpr (imageDir + "surround.jpg") outputFormat "dmsri549,700,549,2427,1289,2427,1289,700"

[<EntryPoint>]
let main argv =
    Console.OutputEncoding <- System.Text.Encoding.UTF8
    let engineFileName = getEngineFileName()
    if String.IsNullOrEmpty(engineFileName) then
        printfn "Unsupported operating system"
        1
    else
        try
            use tsanpr = new TSANPR(engineFileName)

            // TODO: Try each country code as needed
            readLicensePlates tsanpr "KR"
            // readLicensePlates tsanpr "JP"
            // readLicensePlates tsanpr "VN"
            0
        with ex ->
            printfn $"TSANPR initialization failed: {ex.Message}"
            1
