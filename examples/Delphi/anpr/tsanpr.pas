unit tsanpr;

interface

uses
  Windows, SysUtils;

type
  // DLL function pointer types (UTF-8, stdcall)
  _anpr_initialize = function(mode: PUtf8Char): PUtf8Char; stdcall;
  _anpr_read_file = function(imgFileName, outputFormat, options: PUtf8Char): PUtf8Char; stdcall;
  _anpr_read_pixels = function(pixels: Pointer; width, height: LongWord; stride: LongInt;
    pixelFormat, outputFormat, options: PUtf8Char): PUtf8Char; stdcall;

  TTsanprFuncs = record
    anpr_initialize: _anpr_initialize;
    anpr_read_file: _anpr_read_file;
    anpr_read_pixels: _anpr_read_pixels;
  end;
  PTsanprFuncs = ^TTsanprFuncs;

  // DLL load/unload
  function tsanpr_load(tsanpr: PTsanprFuncs; engineFileName: PWideChar): Integer;
  procedure tsanpr_unload;

  // Delphi wrapper functions (string <-> UTF-8)
  function anpr_initialize(tsanpr: PTsanprFuncs; const mode: string): string;
  function anpr_read_file(tsanpr: PTsanprFuncs; const imgFileName, outputFormat, options: string): string;
  function anpr_read_pixels(tsanpr: PTsanprFuncs; pixels: Pointer; width, height: LongWord; stride: LongInt;
    const pixelFormat, outputFormat, options: string): string;

implementation

var
  hDLL: HMODULE = 0;

// Load the DLL and assign function pointers
function tsanpr_load(tsanpr: PTsanprFuncs; engineFileName: PWideChar): Integer;
begin
  hDLL := LoadLibraryW(engineFileName);
  if hDLL = 0 then
    Exit(-1);

  @tsanpr^.anpr_initialize := GetProcAddress(hDLL, 'anpr_initialize');
  @tsanpr^.anpr_read_file := GetProcAddress(hDLL, 'anpr_read_file');
  @tsanpr^.anpr_read_pixels := GetProcAddress(hDLL, 'anpr_read_pixels');

  if not Assigned(tsanpr^.anpr_initialize) or
     not Assigned(tsanpr^.anpr_read_file) or
     not Assigned(tsanpr^.anpr_read_pixels) then
  begin
    FreeLibrary(hDLL);
    hDLL := 0;
    Exit(-2);
  end;

  Result := 0;
end;

// Unload the DLL
procedure tsanpr_unload;
begin
  if hDLL <> 0 then
    FreeLibrary(hDLL);
  hDLL := 0;
end;

// Wrapper for anpr_initialize: Delphi string → UTF-8 → PUtf8Char, result UTF-8 → string
function anpr_initialize(tsanpr: PTsanprFuncs; const mode: string): string;
var
  utf8Mode: UTF8String;
  resultPtr: PUtf8Char;
begin
  utf8Mode := UTF8Encode(mode);
  resultPtr := tsanpr^.anpr_initialize(PUtf8Char(utf8Mode));
  if (resultPtr <> nil) and (resultPtr[0] <> #0) then
    Result := UTF8ToString(resultPtr)
  else
    Result := '';
end;

// Wrapper for anpr_read_file: Delphi string → UTF-8 → PUtf8Char, result UTF-8 → string
function anpr_read_file(tsanpr: PTsanprFuncs; const imgFileName, outputFormat, options: string): string;
var
  utf8Img, utf8Fmt, utf8Opt: UTF8String;
  resultPtr: PUtf8Char;
begin
  utf8Img := UTF8Encode(imgFileName);
  utf8Fmt := UTF8Encode(outputFormat);
  utf8Opt := UTF8Encode(options);
  resultPtr := tsanpr^.anpr_read_file(PUtf8Char(utf8Img), PUtf8Char(utf8Fmt), PUtf8Char(utf8Opt));
  if (resultPtr <> nil) and (resultPtr[0] <> #0) then
    Result := UTF8ToString(resultPtr)
  else
    Result := '';
end;

// Wrapper for anpr_read_pixels: Delphi string → UTF-8 → PUtf8Char, result UTF-8 → string
function anpr_read_pixels(tsanpr: PTsanprFuncs; pixels: Pointer; width, height: LongWord; stride: LongInt;
  const pixelFormat, outputFormat, options: string): string;
var
  utf8PixFmt, utf8OutFmt, utf8Opt: UTF8String;
  resultPtr: PUtf8Char;
begin
  utf8PixFmt := UTF8Encode(pixelFormat);
  utf8OutFmt := UTF8Encode(outputFormat);
  utf8Opt := UTF8Encode(options);
  resultPtr := tsanpr^.anpr_read_pixels(pixels, width, height, stride,
    PUtf8Char(utf8PixFmt), PUtf8Char(utf8OutFmt), PUtf8Char(utf8Opt));
  if (resultPtr <> nil) and (resultPtr[0] <> #0) then
    Result := UTF8ToString(resultPtr)
  else
    Result := '';
end;

end.

