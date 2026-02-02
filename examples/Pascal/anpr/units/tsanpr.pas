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

unit TSANPR;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, DynLibs;

type
  TTSANPRHandle = record
    LibraryHandle: TLibHandle;
    InitFunc: Pointer;
    ReadFileFunc: Pointer;
    ReadPixelsFunc: Pointer;
  end;

  // Function prototypes
  TANPRInitializeFunc = function(const Mode: PChar): PChar; cdecl;
  TANPRReadFileFunc = function(const ImagePath, OutputFormat, Options: PChar): PChar; cdecl;
  TANPRReadPixelsFunc = function(PixelData: Pointer; Width, Height: QWord; 
                                Stride: Int64; const PixelFormat, OutputFormat, Options: PChar): PChar; cdecl;

// Load the TSANPR library
function TSANPRLoadLibrary(const LibraryPath: string): TTSANPRHandle;

// Initialize ANPR engine
function TSANPRInitialize(Handle: TTSANPRHandle; const Mode: string): string;

// Read image file and perform ANPR
function TSANPRReadFile(Handle: TTSANPRHandle; const ImagePath, OutputFormat, Options: string): string;

// Read pixel data and perform ANPR
function TSANPRReadPixels(Handle: TTSANPRHandle; PixelData: Pointer; Width, Height: Cardinal;
                         Stride: Integer; const PixelFormat, OutputFormat, Options: string): string;

// Clean up resources
procedure TSANPRDispose(var Handle: TTSANPRHandle);

implementation

function TSANPRLoadLibrary(const LibraryPath: string): TTSANPRHandle;
begin
  FillChar(Result, SizeOf(Result), 0);
  
  // Load dynamic library
  Result.LibraryHandle := LoadLibrary(PChar(LibraryPath));
  if Result.LibraryHandle = NilHandle then
    raise Exception.Create('Failed to load library: ' + LibraryPath);
  
  // Load function pointers
  Result.InitFunc := GetProcedureAddress(Result.LibraryHandle, 'anpr_initialize');
  Result.ReadFileFunc := GetProcedureAddress(Result.LibraryHandle, 'anpr_read_file');
  Result.ReadPixelsFunc := GetProcedureAddress(Result.LibraryHandle, 'anpr_read_pixels');
  
  if (Result.InitFunc = nil) or (Result.ReadFileFunc = nil) or (Result.ReadPixelsFunc = nil) then
  begin
    UnloadLibrary(Result.LibraryHandle);
    raise Exception.Create('Failed to load TSANPR functions');
  end;
end;

function TSANPRInitialize(Handle: TTSANPRHandle; const Mode: string): string;
var
  InitFunc: TANPRInitializeFunc;
  ResultPtr: PChar;
begin
  if Handle.InitFunc = nil then
  begin
    Result := 'TSANPR not initialized';
    Exit;
  end;
  
  InitFunc := TANPRInitializeFunc(Handle.InitFunc);
  ResultPtr := InitFunc(PChar(Mode));
  
  if ResultPtr = nil then
    Result := ''
  else
    Result := string(ResultPtr);
end;

function TSANPRReadFile(Handle: TTSANPRHandle; const ImagePath, OutputFormat, Options: string): string;
var
  ReadFileFunc: TANPRReadFileFunc;
  ResultPtr: PChar;
begin
  if Handle.ReadFileFunc = nil then
  begin
    Result := 'TSANPR not initialized';
    Exit;
  end;
  
  ReadFileFunc := TANPRReadFileFunc(Handle.ReadFileFunc);
  ResultPtr := ReadFileFunc(PChar(ImagePath), PChar(OutputFormat), PChar(Options));
  
  if ResultPtr = nil then
    Result := 'Failed to process image'
  else
    Result := string(ResultPtr);
end;

function TSANPRReadPixels(Handle: TTSANPRHandle; PixelData: Pointer; Width, Height: Cardinal;
                         Stride: Integer; const PixelFormat, OutputFormat, Options: string): string;
var
  ReadPixelsFunc: TANPRReadPixelsFunc;
  ResultPtr: PChar;
begin
  if Handle.ReadPixelsFunc = nil then
  begin
    Result := 'TSANPR not initialized';
    Exit;
  end;
  
  ReadPixelsFunc := TANPRReadPixelsFunc(Handle.ReadPixelsFunc);
  ResultPtr := ReadPixelsFunc(PixelData, QWord(Width), QWord(Height), Int64(Stride),
                             PChar(PixelFormat), PChar(OutputFormat), PChar(Options));
  
  if ResultPtr = nil then
    Result := 'Failed to process pixels'
  else
    Result := string(ResultPtr);
end;

procedure TSANPRDispose(var Handle: TTSANPRHandle);
begin
  if Handle.LibraryHandle <> NilHandle then
  begin
    UnloadLibrary(Handle.LibraryHandle);
    FillChar(Handle, SizeOf(Handle), 0);
  end;
end;

end.