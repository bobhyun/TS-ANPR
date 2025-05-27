/**
* The MIT License (MIT)
* Copyright © 2022-2025 TS-Solution Corp.
* 
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to all conditions.
* 
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
* 
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
**/

using System;
using System.Runtime.InteropServices;

public class TSANPR : IDisposable
{
    public string anpr_initialize(string mode)
    {
        IntPtr errorPtr = _anpr_initialize(mode);
        string? error = Marshal.PtrToStringUTF8(errorPtr);
        if (string.IsNullOrEmpty(error))
            return "";
        return error;
    }

    public string anpr_read_file(string imgFileName, string outputFormat, string options)
    {
        IntPtr resultPtr = _anpr_read_file(imgFileName, outputFormat, options);
        string? result = Marshal.PtrToStringUTF8(resultPtr);
        if (string.IsNullOrEmpty(result))
            return "";
        return result;
    }

    public string anpr_read_pixels(IntPtr pixels, ulong width, ulong height, long stride,
        string pixelFormat, string outputFormat, string options)
    {
        IntPtr resultPtr = _anpr_read_pixels(pixels, width, height, stride, pixelFormat, outputFormat, options);
        string? result = Marshal.PtrToStringUTF8(resultPtr);
        if (string.IsNullOrEmpty(result))
            return "";
        return result;
    }

    // Delegate definitions matching the native function signatures
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
    private delegate IntPtr AnprInitializeDelegate(string mode);

    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
    private delegate IntPtr AnprReadFileDelegate(string imgFileName, string outputFormat, string options);

    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
    private delegate IntPtr AnprReadPixelsDelegate(
        IntPtr pixels, ulong width, ulong height, long stride,
        string pixelFormat, string outputFormat, string options);

    // Delegate instances (function pointers)
    private AnprInitializeDelegate _anpr_initialize;
    private AnprReadFileDelegate _anpr_read_file;
    private AnprReadPixelsDelegate _anpr_read_pixels;

    // Native library handle
    private IntPtr _libHandle;

    public TSANPR(string libraryPath)
    {
        try
        {
            _libHandle = NativeLibrary.Load(libraryPath);
        }
        catch (Exception ex)
        {
            throw new InvalidOperationException($"Failed to load native library: {libraryPath}\n{ex.Message}", ex);
        }

        try
        {
            _anpr_initialize = GetFunctionDelegate<AnprInitializeDelegate>("anpr_initialize");
            _anpr_read_file = GetFunctionDelegate<AnprReadFileDelegate>("anpr_read_file");
            _anpr_read_pixels = GetFunctionDelegate<AnprReadPixelsDelegate>("anpr_read_pixels");
        }
        catch (Exception ex)
        {
            if (_libHandle != IntPtr.Zero)
            {
                NativeLibrary.Free(_libHandle);
                _libHandle = IntPtr.Zero;
            }
            throw new InvalidOperationException($"Failed to bind native functions from: {libraryPath}\n{ex.Message}", ex);
        }
    }

    // Helper: Get a delegate for a native function
    private T GetFunctionDelegate<T>(string functionName) where T : Delegate
    {
        IntPtr funcPtr = NativeLibrary.GetExport(_libHandle, functionName);
        return Marshal.GetDelegateForFunctionPointer<T>(funcPtr);
    }

    // Unloads the native library.
    public void Dispose()
    {
        if (_libHandle != IntPtr.Zero)
        {
            NativeLibrary.Free(_libHandle);
            _libHandle = IntPtr.Zero;
        }
    }
}
