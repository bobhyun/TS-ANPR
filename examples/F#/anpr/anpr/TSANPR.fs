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

module TSANPR

open System
open System.Runtime.InteropServices

type AnprInitializeDelegate = delegate of string -> IntPtr
type AnprReadFileDelegate = delegate of string * string * string -> IntPtr
type AnprReadPixelsDelegate = delegate of IntPtr * uint64 * uint64 * int64 * string * string * string -> IntPtr

type TSANPR(libraryPath: string) =
    let libHandle = NativeLibrary.Load(libraryPath)

    member private _.getFunctionDelegate<'T when 'T :> Delegate> (funcName: string) : 'T =
        let funcPtr = NativeLibrary.GetExport(libHandle, funcName)
        Marshal.GetDelegateForFunctionPointer(funcPtr, typeof<'T>) :?> 'T

    member this.anpr_initialize(mode: string) : string =
        let del = this.getFunctionDelegate<AnprInitializeDelegate>("anpr_initialize")
        let ptr = del.Invoke(mode)
        let error = Marshal.PtrToStringUTF8(ptr)
        if String.IsNullOrEmpty(error) then "" else error

    member this.anpr_read_file(filename: string, outputFormat: string, options: string) : string =
        let del = this.getFunctionDelegate<AnprReadFileDelegate>("anpr_read_file")
        let ptr = del.Invoke(filename, outputFormat, options)
        let result = Marshal.PtrToStringUTF8(ptr)
        if String.IsNullOrEmpty(result) then "" else result

    member this.anpr_read_pixels(pixels: IntPtr, width: uint64, height: uint64, stride: int64, pixelFormat: string, outputFormat: string, options: string) : string =
        let del = this.getFunctionDelegate<AnprReadPixelsDelegate>("anpr_read_pixels")
        let ptr = del.Invoke(pixels, width, height, stride, pixelFormat, outputFormat, options)
        let result = Marshal.PtrToStringUTF8(ptr)
        if String.IsNullOrEmpty(result) then "" else result

    interface IDisposable with
        member _.Dispose() =
            if libHandle <> IntPtr.Zero then
                NativeLibrary.Free(libHandle)
