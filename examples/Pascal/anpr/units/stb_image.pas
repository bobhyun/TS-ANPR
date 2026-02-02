{
  stb_image Pascal bindings
  Copyright © 2022-2025 TS-Solution Corp.
}

unit stb_image;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DynLibs, ctypes;

type
  TStbImageHandle = record
    LibraryHandle: TLibHandle;
    LoadFunc: Pointer;
    FreeFunc: Pointer;
  end;

  TStbiLoadFunc = function(filename: PChar; x, y, channels_in_file: PInteger;
                          desired_channels: Integer): PByte; cdecl;
  TStbiFreeFunc = procedure(retval_from_stbi_load: Pointer); cdecl;

// Load stb_image library
function StbImageLoadLibrary(const LibraryPath: string): TStbImageHandle;

// Load image from file
function StbiLoad(Handle: TStbImageHandle; const Filename: string;
                  out Width, Height, Channels: Integer;
                  DesiredChannels: Integer): PByte;

// Free image data
procedure StbiImageFree(Handle: TStbImageHandle; Data: Pointer);

// Dispose library
procedure StbImageDispose(var Handle: TStbImageHandle);

implementation

function StbImageLoadLibrary(const LibraryPath: string): TStbImageHandle;
begin
  FillChar(Result, SizeOf(Result), 0);

  Result.LibraryHandle := LoadLibrary(PChar(LibraryPath));
  if Result.LibraryHandle = NilHandle then
    raise Exception.Create('Failed to load stb_image library: ' + LibraryPath);

  Result.LoadFunc := GetProcedureAddress(Result.LibraryHandle, 'stbi_load_export');
  Result.FreeFunc := GetProcedureAddress(Result.LibraryHandle, 'stbi_image_free_export');

  if (Result.LoadFunc = nil) or (Result.FreeFunc = nil) then
  begin
    UnloadLibrary(Result.LibraryHandle);
    raise Exception.Create('Failed to load stb_image functions');
  end;
end;

function StbiLoad(Handle: TStbImageHandle; const Filename: string;
                  out Width, Height, Channels: Integer;
                  DesiredChannels: Integer): PByte;
var
  LoadFunc: TStbiLoadFunc;
begin
  if Handle.LoadFunc = nil then
  begin
    Result := nil;
    Exit;
  end;

  LoadFunc := TStbiLoadFunc(Handle.LoadFunc);
  Result := LoadFunc(PChar(Filename), @Width, @Height, @Channels, DesiredChannels);
end;

procedure StbiImageFree(Handle: TStbImageHandle; Data: Pointer);
var
  FreeFunc: TStbiFreeFunc;
begin
  if (Handle.FreeFunc <> nil) and (Data <> nil) then
  begin
    FreeFunc := TStbiFreeFunc(Handle.FreeFunc);
    FreeFunc(Data);
  end;
end;

procedure StbImageDispose(var Handle: TStbImageHandle);
begin
  if Handle.LibraryHandle <> NilHandle then
  begin
    UnloadLibrary(Handle.LibraryHandle);
    FillChar(Handle, SizeOf(Handle), 0);
  end;
end;

end.
