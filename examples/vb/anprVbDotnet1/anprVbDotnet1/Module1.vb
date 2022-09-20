Imports System
Imports System.Collections.Generic
Imports System.Reflection
Imports System.Runtime.InteropServices
Imports System.Text
Imports System.Net.Mime.MediaTypeNames
Imports System.Drawing

Module Module1

  Dim IMG_PATH As String = "..\..\img\"

  <DllImport("tsanpr.dll", CallingConvention:=CallingConvention.StdCall)>
  Private Function anpr_initialize(
    <MarshalAs(UnmanagedType.LPUTF8Str)> ByVal outputFormat As String     '[IN] 오류 발생시 출력 데이터 형식
  ) As IntPtr
  End Function
  <DllImport("tsanpr.dll", CallingConvention:=CallingConvention.StdCall)>
  Private Function anpr_read_file(
    <MarshalAs(UnmanagedType.LPUTF8Str)> ByVal imgFileName As String,     '[IN] 입력 이미지 파일명
    <MarshalAs(UnmanagedType.LPUTF8Str)> ByVal outputFormat As String,    '[IN] 출력 데이터 형식
    <MarshalAs(UnmanagedType.LPUTF8Str)> ByVal options As String          '[IN] 기능 옵션
  ) As IntPtr
  End Function
  <DllImport("tsanpr.dll", CallingConvention:=CallingConvention.StdCall)>
  Private Function anpr_read_pixels(
    pixels As IntPtr,                                               '[IN] 이미지 픽셀 시작 주소
    <MarshalAs(UnmanagedType.I4)> width As Integer,                 '[IN] 이미지 가로 픽셀 수
    <MarshalAs(UnmanagedType.I4)> height As Integer,                '[IN] 이미지 세로 픽셀 수
    <MarshalAs(UnmanagedType.I4)> stride As Integer,                '[IN] 이미지 한 라인의 바이트 수
    <MarshalAs(UnmanagedType.LPUTF8Str)> pixelFormat As String,     '[IN] 이미지 픽셀 형식
    <MarshalAs(UnmanagedType.LPUTF8Str)> outputFormat As String,    '[IN] 출력 데이터 형식
    <MarshalAs(UnmanagedType.LPUTF8Str)> options As String          '[IN] 기능 옵션
  ) As IntPtr
  End Function

  Private Sub readFile(ByVal imgfile As String, ByVal outputFormat As String, ByVal options As String)
    Console.Write("{0} (outputFormat=""{1}"", options=""{2}"") => ", imgfile, outputFormat, options)
    Dim result As IntPtr = anpr_read_file(imgfile, outputFormat, options)
    Dim str As String = ptrToUtf8(result)
    Console.WriteLine(str)
  End Sub

  Private Sub readPixels(ByVal imgfile As String, ByVal outputFormat As String, ByVal options As String)
    Console.Write("{0} (outputFormat=""{1}"", options=""{2}"") => ", imgfile, outputFormat, options)
    Dim bmp As Bitmap = New Bitmap(imgfile)
    Dim rect As Rectangle = New Rectangle(0, 0, bmp.Width, bmp.Height)
    Dim bmpData As Imaging.BitmapData = bmp.LockBits(rect, Imaging.ImageLockMode.ReadWrite, bmp.PixelFormat)
    Dim result As IntPtr = anpr_read_pixels(bmpData.Scan0, bmpData.Width, bmpData.Height, bmpData.Stride, "BGR", outputFormat, options)
    Dim str As String = ptrToUtf8(result)
    Console.WriteLine(str)
  End Sub

  Private Function ptrToUtf8(ByVal nativeUtf8 As IntPtr) As String
    If nativeUtf8 = IntPtr.Zero Then
      Return String.Empty
    End If

    Dim len As Integer = 0
    While Marshal.ReadByte(nativeUtf8, len) <> 0
      len += 1
    End While

    Dim buffer As Byte() = New Byte(len - 1) {}
    Marshal.Copy(nativeUtf8, buffer, 0, buffer.Length)
    Dim str As String = Encoding.UTF8.GetString(buffer)
    buffer = Nothing
    Return str
  End Function

  Private Sub anprDemo1(ByVal outputFormat As String)
    readFile(IMG_PATH + "licensePlate.jpg", outputFormat, "v")
    readFile(IMG_PATH + "licensePlate.jpg", outputFormat, "")
    readFile(IMG_PATH + "multiple.jpg", outputFormat, "vm")
    readFile(IMG_PATH + "multiple.jpg", outputFormat, "")
    readFile(IMG_PATH + "surround.jpg", outputFormat, "vms")
    readFile(IMG_PATH + "surround.jpg", outputFormat, "")
  End Sub

  Private Sub anprDemo2(ByVal outputFormat As String)
    readPixels(IMG_PATH & "licensePlate.jpg", outputFormat, "v")
    readPixels(IMG_PATH & "licensePlate.jpg", outputFormat, "")
    readPixels(IMG_PATH & "multiple.jpg", outputFormat, "vm")
    readPixels(IMG_PATH & "multiple.jpg", outputFormat, "")
    readPixels(IMG_PATH & "surround.jpg", outputFormat, "vms")
    readPixels(IMG_PATH & "surround.jpg", outputFormat, "")
  End Sub
  Sub Main()
    Console.OutputEncoding = System.Text.Encoding.UTF8

    Dim result As IntPtr = anpr_initialize("text")
    Dim str As String = ptrToUtf8(result)
    If Not str = String.Empty Then
      Console.WriteLine(err)
      Return
    End If

    anprDemo1("text")
    anprDemo1("json")
    anprDemo1("yaml")
    anprDemo1("xml")
    anprDemo1("csv")

    anprDemo2("text")
    anprDemo2("json")
    anprDemo2("yaml")
    anprDemo2("xml")
    anprDemo2("csv")
  End Sub

End Module
