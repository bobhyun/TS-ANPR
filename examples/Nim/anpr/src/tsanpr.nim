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

import dynlib, os

type
  TSANPR* = ref object
    lib: LibHandle

proc cleanupTSANPR(tsanpr: TSANPR) =
  ## Destructor - unload library when object is destroyed
  if tsanpr.lib != nil:
    unloadLib(tsanpr.lib)

proc newTSANPR*(libraryPath: string): TSANPR =
  ## Initialize TSANPR with the given library path.
  if not fileExists(libraryPath):
    raise newException(IOError, "Library file not found: " & libraryPath)

  let lib = loadLib(libraryPath)
  if lib == nil:
    raise newException(OSError, "Failed to load native library: " & libraryPath)

  new(result, cleanupTSANPR)
  result.lib = lib

proc anprInitialize*(tsanpr: TSANPR, mode: string): string =
  ## Initialize the ANPR engine with the specified mode.
  try:
    let anpr_init = cast[proc(mode: cstring): cstring {.cdecl.}](tsanpr.lib.symAddr("anpr_initialize"))
    if anpr_init == nil:
      return "Function anpr_initialize not found"
    
    let result_ptr = anpr_init(mode.cstring)
    if result_ptr == nil:
      return ""
    else:
      return $result_ptr
  except:
    return "Function call failed: " & getCurrentExceptionMsg()

proc anprReadFile*(tsanpr: TSANPR, imgFileName: string, outputFormat: string, options: string): string =
  ## Read and process an image file.
  try:
    let anpr_read = cast[proc(imgFileName: cstring, outputFormat: cstring, options: cstring): cstring {.cdecl.}](
      tsanpr.lib.symAddr("anpr_read_file"))
    if anpr_read == nil:
      return "Function anpr_read_file not found"
    
    let result_ptr = anpr_read(imgFileName.cstring, outputFormat.cstring, options.cstring)
    if result_ptr == nil:
      return ""
    else:
      return $result_ptr
  except:
    return "Function call failed: " & getCurrentExceptionMsg()

proc anprReadPixels*(tsanpr: TSANPR, pixels: ptr UncheckedArray[uint8], width: uint64, 
                    height: uint64, stride: int64, pixelFormat: string, 
                    outputFormat: string, options: string): string =
  ## Process pixel data directly.
  try:
    let anpr_pixels = cast[proc(pixels: ptr UncheckedArray[uint8], width: uint64, height: uint64, 
                               stride: int64, pixelFormat: cstring, outputFormat: cstring, 
                               options: cstring): cstring {.cdecl.}](
      tsanpr.lib.symAddr("anpr_read_pixels"))
    if anpr_pixels == nil:
      return "Function anpr_read_pixels not found"
    
    let result_ptr = anpr_pixels(pixels, width, height, stride, pixelFormat.cstring, 
                                outputFormat.cstring, options.cstring)
    if result_ptr == nil:
      return ""
    else:
      return $result_ptr
  except:
    return "Function call failed: " & getCurrentExceptionMsg()