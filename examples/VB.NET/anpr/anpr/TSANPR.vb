' The MIT License (MIT)
' Copyright © 2022-2025 TS-Solution Corp.
' 
' Permission Is hereby granted, free of charge, to any person obtaining a copy
' of this software And associated documentation files (the "Software"), to deal
' in the Software without restriction, including without limitation the rights
' to use, copy, modify, merge, publish, distribute, sublicense, And/Or sell
' copies of the Software, And to permit persons to whom the Software Is
' furnished to do so, subject to all conditions.
' 
' The above copyright notice And this permission notice shall be included in all
' copies Or substantial portions of the Software.
' 
' THE SOFTWARE Is PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS Or
' IMPLIED, INCLUDING BUT Not LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
' FITNESS FOR A PARTICULAR PURPOSE And NONINFRINGEMENT. IN NO EVENT SHALL THE
' AUTHORS Or COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES Or OTHER
' LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT Or OTHERWISE, ARISING FROM,
' OUT OF Or IN CONNECTION WITH THE SOFTWARE Or THE USE Or OTHER DEALINGS IN THE
' SOFTWARE.

Imports System
Imports System.Runtime.InteropServices

' TSANPR: Dynamic wrapper for native ANPR library
Public Class TSANPR
  Implements IDisposable

  ' Delegate definitions matching the native function signatures
  <UnmanagedFunctionPointer(CallingConvention.Cdecl)>
  Private Delegate Function AnprInitializeDelegate(mode As String) As IntPtr

  <UnmanagedFunctionPointer(CallingConvention.Cdecl)>
  Private Delegate Function AnprReadFileDelegate(imgFileName As String, outputFormat As String, options As String) As IntPtr

  <UnmanagedFunctionPointer(CallingConvention.Cdecl)>
  Private Delegate Function AnprReadPixelsDelegate(
      pixels As IntPtr,
      width As ULong,
      height As ULong,
      stride As Long,
      pixelFormat As String,
      outputFormat As String,
      options As String
  ) As IntPtr

  ' Delegate instances (function pointers)
  Private _anpr_initialize As AnprInitializeDelegate
  Private _anpr_read_file As AnprReadFileDelegate
  Private _anpr_read_pixels As AnprReadPixelsDelegate

  ' Native library handle
  Private _libHandle As IntPtr

  ' Constructor: Loads the native library and binds function pointers
  Public Sub New(libraryPath As String)
    Try
      _libHandle = NativeLibrary.Load(libraryPath)
    Catch ex As Exception
      Throw New InvalidOperationException($"Failed to load native library: {libraryPath}{vbLf}{ex.Message}", ex)
    End Try

    Try
      _anpr_initialize = GetFunctionDelegate(Of AnprInitializeDelegate)("anpr_initialize")
      _anpr_read_file = GetFunctionDelegate(Of AnprReadFileDelegate)("anpr_read_file")
      _anpr_read_pixels = GetFunctionDelegate(Of AnprReadPixelsDelegate)("anpr_read_pixels")
    Catch ex As Exception
      If _libHandle <> IntPtr.Zero Then
        NativeLibrary.Free(_libHandle)
        _libHandle = IntPtr.Zero
      End If
      Throw New InvalidOperationException($"Failed to bind native functions from: {libraryPath}{vbLf}{ex.Message}", ex)
    End Try
  End Sub

  ' Helper: Gets a delegate for a native function (no type constraint in VB.NET)
  Private Function GetFunctionDelegate(Of T)(functionName As String) As T
    Dim funcPtr = NativeLibrary.GetExport(_libHandle, functionName)
    Return CType(CType(Marshal.GetDelegateForFunctionPointer(funcPtr, GetType(T)), Object), T)
  End Function

  ' Calls the native anpr_initialize function
  Public Function anpr_initialize(mode As String) As String
    Dim errorPtr = _anpr_initialize(mode)
    Dim [error] = Marshal.PtrToStringUTF8(errorPtr)
    If String.IsNullOrEmpty([error]) Then
      Return ""
    End If
    Return [error]
  End Function

  ' Calls the native anpr_read_file function
  Public Function anpr_read_file(imgFileName As String, outputFormat As String, options As String) As String
    Dim resultPtr = _anpr_read_file(imgFileName, outputFormat, options)
    Dim result = Marshal.PtrToStringUTF8(resultPtr)
    If String.IsNullOrEmpty(result) Then
      Return ""
    End If
    Return result
  End Function

  ' Calls the native anpr_read_pixels function
  Public Function anpr_read_pixels(
      pixels As IntPtr,
      width As ULong,
      height As ULong,
      stride As Long,
      pixelFormat As String,
      outputFormat As String,
      options As String
  ) As String
    Dim resultPtr = _anpr_read_pixels(pixels, width, height, stride, pixelFormat, outputFormat, options)
    Dim result = Marshal.PtrToStringUTF8(resultPtr)
    If String.IsNullOrEmpty(result) Then
      Return ""
    End If
    Return result
  End Function

  ' Unloads the native library
  Public Sub Dispose() Implements IDisposable.Dispose
    If _libHandle <> IntPtr.Zero Then
      NativeLibrary.Free(_libHandle)
      _libHandle = IntPtr.Zero
    End If
    GC.SuppressFinalize(Me)
  End Sub

End Class
