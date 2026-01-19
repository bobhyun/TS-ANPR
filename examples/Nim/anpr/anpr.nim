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

import os, strutils
import tsanpr

proc getExamplesBaseDir(): string =
  ## Get the examples base directory at runtime.
  parentDir(parentDir(getCurrentDir()))

proc getEngineFileName(): string =
  ## Generate engine filename depending on platform and architecture.
  let baseDir = getExamplesBaseDir()
  when defined(windows):
    when defined(amd64):
      result = baseDir / "bin" / "windows-x86_64" / "tsanpr.dll"
    else:
      result = baseDir / "bin" / "windows-x86" / "tsanpr.dll"
  elif defined(linux):
    when defined(amd64):
      result = baseDir / "bin" / "linux-x86_64" / "libtsanpr.so"
    elif defined(arm64):
      result = baseDir / "bin" / "linux-aarch64" / "libtsanpr.so"
    else:
      result = ""
  else:
    result = ""

proc readImageFile(tsanpr: TSANPR, imgfile: string, outputFormat: string, options: string) =
  ## Read an image file and call anpr_read_file.
  stdout.write("$1 (outputFormat=\"$2\", options=\"$3\") => " % [imgfile, outputFormat, options])
  let result = tsanpr.anprReadFile(imgfile, outputFormat, options)
  echo result

proc readEncodedImage(tsanpr: TSANPR, imgfile: string, outputFormat: string, options: string) =
  ## Read an encoded image file as bytes and call tsanpr.anprReadPixels with 'encoded' pixel format.
  stdout.write("$1 (outputFormat=\"$2\", options=\"$3\") => " % [imgfile, outputFormat, options])
  
  try:
    if not fileExists(imgfile):
      echo "File does not exist"
      return
    
    let encodedImg = readFile(imgfile)
    let result = tsanpr.anprReadPixels(
      cast[ptr UncheckedArray[uint8]](encodedImg[0].unsafeAddr),
      encodedImg.len.uint64,
      0'u64,
      0'i64,
      "encoded",
      outputFormat,
      options
    )
    echo result
  except:
    echo "ERROR: Exception - ", getCurrentExceptionMsg()

proc getPixelFormat(channels: int): string =
  ## Determine pixel format string based on image channels.
  case channels:
    of 1: "GRAY"
    of 2: "BGR565"  # or "BGR555"
    of 3: "BGR"
    of 4: "BGRA"
    else: ""

proc readPixelBuffer(tsanpr: TSANPR, imgfile: string, outputFormat: string, options: string) =
  ## Use the pixel buffer-based ANPR function.
  ## Note: This is a simplified version. In a real implementation,
  ## you would need to integrate with an image processing library.
  stdout.write("$1 (outputFormat=\"$2\", options=\"$3\") => " % [imgfile, outputFormat, options])
  echo "Pixel buffer reading not implemented in this example"

proc readLicensePlates(tsanpr: TSANPR, countryCode: string) =
  ## NOTICE:
  ## anpr_initialize should be called only once after library load.
  ## Therefore, it is not possible to change the country code after anpr_initialize has been called.
  ## While using the free trial license, you can try all languages.
  ## When you purchase a commercial license, you can only use the selected language.
  let error = tsanpr.anprInitialize("text;country=" & countryCode)
  if error.len > 0:
    echo "anpr_initialize() failed: ", error
    return

  let imageDir = getExamplesBaseDir() / "img" / countryCode
  
  # TODO: Try each function as needed
  let anprFunc = readImageFile
  # let anprFunc = readEncodedImage
  # let anprFunc = readPixelBuffer
  
  # TODO: Try each output format as needed
  let outputFormat = "text"
  # let outputFormat = "json"
  # let outputFormat = "yaml"
  # let outputFormat = "xml"
  # let outputFormat = "csv"
  
  anprFunc(tsanpr, imageDir / "licensePlate.jpg", outputFormat, "")   # Single license plate recognition (default)
  anprFunc(tsanpr, imageDir / "multiple.jpg", outputFormat, "vm")     # Recognize multiple license plates attached to vehicles
  anprFunc(tsanpr, imageDir / "multiple.jpg", outputFormat, "vmb")    # Recognize multiple license plates attached to vehicles (including motorcycles)
  anprFunc(tsanpr, imageDir / "surround.jpg", outputFormat, "vms")    # Recognize multiple license plates attached to vehicles with surround detection
  anprFunc(tsanpr, imageDir / "surround.jpg", outputFormat, "dms")    # Recognize multiple surrounding objects (vehicles)
  anprFunc(tsanpr, imageDir / "surround.jpg", outputFormat, "dmsr")   # Recognize multiple surrounding objects (vehicles) and license plates
  
  # Recognize multiple surrounding objects and license plates within RoI
  anprFunc(tsanpr, imageDir / "surround.jpg", outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700")

proc main() =
  let engineFileName = getEngineFileName()
  if engineFileName.len == 0 or not fileExists(engineFileName):
    echo "Unsupported operating system or engine not found"
    return
  
  try:
    let tsanpr = newTSANPR(engineFileName)
    
    # TODO: Try each country code as needed
    readLicensePlates(tsanpr, "KR")
    # readLicensePlates(tsanpr, "JP")
    # readLicensePlates(tsanpr, "VN")
    
  except:
    echo "TSANPR initialization failed: ", getCurrentExceptionMsg()

when isMainModule:
  main()