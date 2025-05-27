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
Imports System.IO
Imports System.Runtime.InteropServices
Imports OpenCvSharp

Module Program

  Private tsanpr As TSANPR = Nothing
  Private ReadOnly examplesBaseDir As String = "../../../../../.."

  ' Determine engine file path based on OS and architecture
  Function GetEngineFileName() As String
    Dim arch = RuntimeInformation.ProcessArchitecture
    If RuntimeInformation.IsOSPlatform(OSPlatform.Windows) Then
      Select Case arch
        Case Architecture.X86
          Return examplesBaseDir & "/bin/windows-x86/tsanpr.dll"
        Case Architecture.X64
          Return examplesBaseDir & "/bin/windows-x86_64/tsanpr.dll"
      End Select
    ElseIf RuntimeInformation.IsOSPlatform(OSPlatform.Linux) Then
      Select Case arch
        Case Architecture.X64
          Return examplesBaseDir & "/bin/linux-x86_64/libtsanpr.so"
        Case Architecture.Arm64
          Return examplesBaseDir & "/bin/linux-aarch64/libtsanpr.so"
      End Select
    End If
    Return ""
  End Function

  Delegate Sub AnprReadDelegate(imgfile As String, outputFormat As String, options As String)

  ' Read license plate from image file
  Sub ReadImageFile(imgfile As String, outputFormat As String, options As String)
    If tsanpr Is Nothing Then Return
    Console.Write($"{imgfile} (outputFormat=""{outputFormat}"", options=""{options}"") => ")
    Dim result = tsanpr.anpr_read_file(imgfile, outputFormat, options)
    Console.WriteLine(result)
  End Sub

  ' Read license plate from encoded image bytes
  Sub ReadEncodedImage(imgfile As String, outputFormat As String, options As String)
    If tsanpr Is Nothing Then Return
    Console.Write($"{imgfile} (outputFormat=""{outputFormat}"", options=""{options}"") => ")
    Try
      Dim encodedImg = File.ReadAllBytes(imgfile)
      Dim handle = GCHandle.Alloc(encodedImg, GCHandleType.Pinned)
      Try
        Dim ptr = handle.AddrOfPinnedObject()
        Dim result = tsanpr.anpr_read_pixels(
            ptr,
            CLng(encodedImg.Length),
            0,
            0,
            "encoded",
            outputFormat,
            options
        )
        Console.WriteLine(result)
      Finally
        handle.Free()
      End Try
    Catch ex As Exception
      Console.Error.WriteLine($"\nERROR: Exception - {ex.Message}")
    End Try
  End Sub

  ' Get pixel format string based on image channels
  Function GetPixelFormat(img As Mat) As String
    Dim channels = img.Channels()
    Select Case channels
      Case 1 : Return "GRAY"
      Case 2 : Return "BGR565"
      Case 3 : Return "BGR"
      Case 4 : Return "BGRA"
      Case Else : Return Nothing
    End Select
  End Function

  ' Read license plate from raw pixel buffer
  Sub ReadPixelBuffer(imgfile As String, outputFormat As String, options As String)
    If tsanpr Is Nothing Then Return
    Console.Write($"{imgfile} (outputFormat=""{outputFormat}"", options=""{options}"") => ")
    Using img = Cv2.ImRead(imgfile, ImreadModes.Unchanged)
      If img.Empty() Then
        Console.Error.WriteLine("Image load failed!")
        Return
      End If
      Dim pixelFormat = GetPixelFormat(img)
      If pixelFormat Is Nothing Then
        Console.Error.WriteLine("Unknown pixel format!")
        Return
      End If
      Dim imgData = img.Data
      Dim stride = CInt(img.Step())
      Dim result = tsanpr.anpr_read_pixels(
          imgData,
          CULng(img.Width),
          CULng(img.Height),
          stride,
          pixelFormat,
          outputFormat,
          options
      )
      Console.WriteLine(result)
    End Using
  End Sub

  ' Run license plate recognition for a specific country
  Sub readLicensePlates(countryCode As String)
    If tsanpr Is Nothing Then Return
    
    ' NOTICE:
    ' anpr_initialize should be called only once after library load.
    ' Therefore, it is not possible to change the country code after anpr_initialize has been called.
    ' While using the free trial license, you can try all languages.
    ' When you purchase a commercial license, you can only use the selected language.    
    Dim [error] = tsanpr.anpr_initialize("text;country=" & countryCode)
    If Not String.IsNullOrEmpty([error]) Then
      Console.WriteLine($"anpr_initialize() failed: {[error]}")
      Return
    End If

    Dim imageDir = examplesBaseDir & "/img/" & countryCode & "/"
    
    ' TODO: Try each function as needed
    Dim anprDelegate As AnprReadDelegate = AddressOf ReadImageFile
    ' Dim anprDelegate As AnprReadDelegate = AddressOf ReadEncodedImage
    ' Dim anprDelegate As AnprReadDelegate = AddressOf ReadPixelBuffer

    ' TODO: Try each output format as needed
    Const outputFormat As String = "text"
    ' Const outputFormat As String = "json"
    ' Const outputFormat As String = "yaml"
    ' Const outputFormat As String = "xml"
    ' Const outputFormat As String = "csv"

    anprDelegate(imageDir & "licensePlate.jpg", outputFormat, "") ' Single license plate recognition (default)
    anprDelegate(imageDir & "multiple.jpg", outputFormat, "vm")   ' Recognize multiple license plates attached to vehicles
    anprDelegate(imageDir & "multiple.jpg", outputFormat, "vmb")  ' Recognize multiple license plates attached to vehicles (including motorcycles)
    anprDelegate(imageDir & "surround.jpg", outputFormat, "vms")  ' Recognize multiple license plates attached to vehicles with surround detection
    anprDelegate(imageDir & "surround.jpg", outputFormat, "dms")  ' Recognize multiple surrounding objects (vehicles)
    anprDelegate(imageDir & "surround.jpg", outputFormat, "dmsr") ' Recognize multiple surrounding objects (vehicles) and license plates
  End Sub

  Sub Main(args As String())
    Console.OutputEncoding = System.Text.Encoding.UTF8
    Dim engineFileName = GetEngineFileName()
    If String.IsNullOrEmpty(engineFileName) Then
      Console.WriteLine("Unsupported operating system")
      Return
    End If

    Try
      tsanpr = New TSANPR(engineFileName)
    Catch ex As Exception
      Console.WriteLine($"TSANPR initialization failed: {ex.Message}")
      Return
    End Try

    ' TODO: Try each country code as needed
    readLicensePlates("KR")
    ' readLicensePlates("JP")
    ' readLicensePlates("VN")
  End Sub

End Module
