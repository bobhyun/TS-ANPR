{
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
}

program ANPR;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Math, TSANPR, stb_image;

const
  // Executable is in bin/, so go up 3 levels to reach examples/
  ExamplesBaseDir = '../../..';

function GetStbImageLibraryPath: string;
var
  OS: string;
begin
  OS := GetEnvironmentVariable('OS');
  if (Length(OS) >= 7) and (Copy(OS, 1, 7) = 'Windows') then
    Result := 'stb_image.dll'
  else
    Result := './libstb_image.so';
end;

function GetEngineFileName: string;
var
  OS, Arch: string;
begin
  OS := GetEnvironmentVariable('OS');

  if (Length(OS) >= 7) and (Copy(OS, 1, 7) = 'Windows') then
  begin
    Arch := GetEnvironmentVariable('PROCESSOR_ARCHITECTURE');
    if (Arch = 'AMD64') or (Arch = 'x86_64') then
      Result := ExamplesBaseDir + '/bin/windows-x86_64/tsanpr.dll'
    else
      Result := ExamplesBaseDir + '/bin/windows-x86/tsanpr.dll';
  end
  else
  begin
    // Linux: check multiple environment variables for architecture
    Arch := GetEnvironmentVariable('HOSTTYPE');
    if Arch = '' then
      Arch := GetEnvironmentVariable('CPU');

    {$IFDEF CPUAARCH64}
    Result := ExamplesBaseDir + '/bin/linux-aarch64/libtsanpr.so';
    {$ELSE}
    if (Arch = 'aarch64') or (Arch = 'arm64') then
      Result := ExamplesBaseDir + '/bin/linux-aarch64/libtsanpr.so'
    else
      Result := ExamplesBaseDir + '/bin/linux-x86_64/libtsanpr.so';
    {$ENDIF}
  end;
end;

procedure ReadImageFile(TS: TTSANPRHandle; const ImgFile, OutputFormat, Options: string);
var
  Result: string;
begin
  Write(ImgFile + ' (outputFormat="' + OutputFormat + '", options="' + Options + '") => ');
  Result := TSANPRReadFile(TS, ImgFile, OutputFormat, Options);
  WriteLn(Result);
end;

procedure ReadEncodedImage(TS: TTSANPRHandle; const ImgFile, OutputFormat, Options: string);
var
  FileStream: TFileStream;
  Buffer: array of Byte;
  FileSize: Int64;
  ResultStr: string;
begin
  Write(ImgFile + ' (outputFormat="' + OutputFormat + '", options="' + Options + '") => ');

  if not FileExists(ImgFile) then
  begin
    WriteLn('File does not exist');
    Exit;
  end;

  try
    FileStream := TFileStream.Create(ImgFile, fmOpenRead or fmShareDenyWrite);
    try
      FileSize := FileStream.Size;
      SetLength(Buffer, FileSize);
      FileStream.ReadBuffer(Buffer[0], FileSize);
    finally
      FileStream.Free;
    end;

    // Call TSANPRReadPixels with "encoded" pixel format
    ResultStr := TSANPRReadPixels(TS, @Buffer[0], FileSize, 0, 0, 'encoded', OutputFormat, Options);
    WriteLn(ResultStr);
  except
    on E: Exception do
      WriteLn('Error reading file: ' + E.Message);
  end;
end;

var
  StbImage: TStbImageHandle;
  StbImageLoaded: Boolean = False;

procedure ReadPixelBuffer(TS: TTSANPRHandle; const ImgFile, OutputFormat, Options: string);
var
  Pixels: PByte;
  Width, Height, Channels: Integer;
  Stride: Integer;
  ResultStr: string;
begin
  Write(ImgFile + ' (outputFormat="' + OutputFormat + '", options="' + Options + '") => ');

  // Load stb_image library if not already loaded
  if not StbImageLoaded then
  begin
    try
      StbImage := StbImageLoadLibrary(GetStbImageLibraryPath);
      StbImageLoaded := True;
    except
      on E: Exception do
      begin
        WriteLn('stb_image library not found - use ReadEncodedImage instead');
        Exit;
      end;
    end;
  end;

  // Load image with 3 channels (RGB)
  Pixels := StbiLoad(StbImage, ImgFile, Width, Height, Channels, 3);
  if Pixels = nil then
  begin
    WriteLn('Failed to load image');
    Exit;
  end;

  try
    Stride := Width * 3;  // 3 bytes per pixel (RGB)
    ResultStr := TSANPRReadPixels(TS, Pixels, Width, Height, Stride, 'RGB', OutputFormat, Options);
    WriteLn(ResultStr);
  finally
    StbiImageFree(StbImage, Pixels);
  end;
end;

procedure ReadLicensePlates(TS: TTSANPRHandle; const CountryCode: string);
var
  ErrorMsg: string;
  ImageDir: string;
  OutputFormat: string;
  
  // TODO: Try each function as needed
  procedure ANPRFunc(const ImgFile, OutputFormat, Options: string);
  begin
    ReadImageFile(TS, ImgFile, OutputFormat, Options);
    // ReadEncodedImage(TS, ImgFile, OutputFormat, Options);
    // ReadPixelBuffer(TS, ImgFile, OutputFormat, Options);
  end;
  
begin
  // NOTICE:
  // anpr_initialize should be called only once after library load.
  // Therefore, it is not possible to change the country code after anpr_initialize has been called.
  // While using the free trial license, you can try all languages.
  // When you purchase a commercial license, you can only use the selected language.
  
  ErrorMsg := TSANPRInitialize(TS, 'text;country=' + CountryCode);
  if ErrorMsg <> '' then
  begin
    WriteLn('anpr_initialize() failed: ' + ErrorMsg);
    Exit;
  end;
  
  ImageDir := ExamplesBaseDir + '/img/' + CountryCode + '/';
  
  // TODO: Try each output format as needed
  OutputFormat := 'text';
  // OutputFormat := 'json';
  // OutputFormat := 'yaml';
  // OutputFormat := 'xml';
  // OutputFormat := 'csv';
  
  // Single license plate recognition (default)
  ANPRFunc(ImageDir + 'licensePlate.jpg', OutputFormat, '');
  
  // Recognize multiple license plates attached to vehicles
  ANPRFunc(ImageDir + 'multiple.jpg', OutputFormat, 'vm');
  
  // Recognize multiple license plates attached to vehicles (including motorcycles)
  ANPRFunc(ImageDir + 'multiple.jpg', OutputFormat, 'vmb');
  
  // Recognize multiple license plates attached to vehicles with surround detection
  ANPRFunc(ImageDir + 'surround.jpg', OutputFormat, 'vms');
  
  // Recognize multiple surrounding objects (vehicles)
  ANPRFunc(ImageDir + 'surround.jpg', OutputFormat, 'dms');
  
  // Recognize multiple surrounding objects (vehicles) and license plates
  ANPRFunc(ImageDir + 'surround.jpg', OutputFormat, 'dmsr');
  
  // Recognize multiple surrounding objects and license plates within RoI
  ANPRFunc(ImageDir + 'surround.jpg', OutputFormat, 'dmsri549,700,549,2427,1289,2427,1289,700');
end;

var
  EngineFileName: string;
  TS: TTSANPRHandle;
  SavedFPUMask: TFPUExceptionMask;

begin
  // Disable floating point exceptions for compatibility with native C library
  SavedFPUMask := SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

  EngineFileName := GetEngineFileName;
  if (EngineFileName = '') or not FileExists(EngineFileName) then
  begin
    WriteLn('Unsupported operating system or engine not found');
    Exit;
  end;
  
  WriteLn('engine file name: ' + EngineFileName);
  
  try
    TS := TSANPRLoadLibrary(EngineFileName);
  except
    on E: Exception do
    begin
      WriteLn('TSANPR initialization failed: ' + E.Message);
      Exit;
    end;
  end;
  
  try
    // TODO: Try each country code as needed
    ReadLicensePlates(TS, 'KR');
    // ReadLicensePlates(TS, 'JP');
    // ReadLicensePlates(TS, 'VN');
  finally
    TSANPRDispose(TS);
    // Dispose stb_image library if loaded
    if StbImageLoaded then
      StbImageDispose(StbImage);
    // Restore original FPU exception mask
    SetExceptionMask(SavedFPUMask);
  end;
end.