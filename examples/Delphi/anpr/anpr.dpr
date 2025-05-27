program anpr;

{$APPTYPE CONSOLE}

uses
   Windows,  System.SysUtils, System.Classes, System.IOUtils,
   Vcl.Graphics, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage,
   System.NetEncoding, tsanpr;

var
{$IFDEF MSWINDOWS}
  examplesBaseDir: string = '..\..\..\..';
{$ELSE}
  examplesBaseDir: string = '../../../..';
{$ENDIF}
  tsanpr: TTsanprFuncs;

function getEngineFileName: string;
begin
  {$IFDEF MSWINDOWS}
    {$IFDEF WIN64}
      // 64-bit Windows
      Result := examplesBaseDir + '\bin\windows-x86_64\tsanpr.dll';
    {$ELSE}
      // 32-bit Windows
      Result := examplesBaseDir + '\bin\windows-x86\tsanpr.dll';
    {$ENDIF}
  {$ELSE}
    {$IFDEF LINUX}
      {$IFDEF CPUX64}
        // 64-bit Linux
        Result := examplesBaseDir + '/bin/linux-x86_64/libtsanpr.so';
      {$ELSEIF Defined(CPUAARCH64)}
        // 64-bit ARM Linux
        Result := examplesBaseDir + '/bin/linux-aarch64/libtsanpr.so';
      {$ELSE}
        Result := '';
      {$ENDIF}
    {$ELSE}
      Result := '';
    {$ENDIF}
  {$ENDIF}
end;

// Wrapper-based image file reading
procedure readImageFile(const imgFile, outputFormat, options: string);
var
  resultStr: string;
begin
  Write(imgFile, ' (outputFormat="', outputFormat, '", options="', options, '") => ');
  resultStr := anpr_read_file(@tsanpr, imgFile, outputFormat, options);
  Writeln(resultStr)
end;

procedure readEncodedImage(const imgFile, outputFormat, options: string);
var
  fs: TFileStream;
  buffer: Pointer;
  size: Integer;
  resultStr: string;
begin
  Write(imgFile, ' (outputFormat="', outputFormat, '", options="', options, '") => ');
  // Open the image file as a binary stream
  fs := TFileStream.Create(imgFile, fmOpenRead or fmShareDenyWrite);
  try
    size := fs.Size;
    // Allocate memory buffer to hold the entire file
    GetMem(buffer, size);
    try
      // Read the whole file into the buffer
      fs.ReadBuffer(buffer^, size);

      resultStr := anpr_read_pixels(@tsanpr, buffer, size, 0, 0, 'encoded', outputFormat, options);

      Writeln(resultStr);
    finally
      FreeMem(buffer);
    end;
  finally
    fs.Free;
  end;
end;

procedure readPixelBuffer(const imgFile, outputFormat, options: string);
var
  bmp: TBitmap;
  jpg: TJPEGImage;
  png: TPngImage;
  pixelBuffer: Pointer;
  stride: Integer;
  width, height: Integer;
  resultStr: string;
  ext: string;
begin
  Write(imgFile, ' (outputFormat="', outputFormat, '", options="', options, '") => ');
  ext := LowerCase(ExtractFileExt(imgFile));
  bmp := TBitmap.Create;
  try
    if ext = '.jpg' then
    begin
      jpg := TJPEGImage.Create;
      try
        jpg.LoadFromFile(imgFile);
        bmp.Assign(jpg);
      finally
        jpg.Free;
      end;
    end
    else if ext = '.png' then
    begin
      png := TPngImage.Create;
      try
        png.LoadFromFile(imgFile);
        bmp.Assign(png);
      finally
        png.Free;
      end;
    end
    else
      bmp.LoadFromFile(imgFile);

    {
      NOTICE:
      The memory layout of a BMP image is such that the image appears vertically flipped;
      scanning proceeds from the bottom of the image upwards.

      Memory buffer start address -> +-------------------------------+
                                     |                               |
                                  ^  |                               |
                                  |  |                               |
                                  |  |                               |
                                  |  |                               |
                 Scan direction   |  |                               |
                                     |                               |
          First scanline address  -> +-------------------------------+
    }
    width := bmp.Width;
    height := bmp.Height;
    // Obtain the starting address of the first scanline of the image
    pixelBuffer := bmp.ScanLine[height - 1];
    stride := Integer(bmp.ScanLine[height - 2]) - Integer(pixelBuffer);

    // Calculate the number of bytes to the next scanline (stride).
    // Since the BMP image memory layout is bottom-up, the stride is negative.
    resultStr := anpr_read_pixels(@tsanpr, pixelBuffer, width, height, stride, 'BGR', outputFormat, options);
    Writeln(resultStr);
  finally
    bmp.Free;
  end;
end;


function readLicensePlates(const countryCode: string): Integer;
var
  initParams: string;
  error: string;
  imageDir: string;
  anprFunc: procedure(const imgFile, outputFormat, options: string);
  outputFormat: string;
begin
  {
    NOTICE:
    anpr_initialize should be called only once after library load.
    Therefore, it is not possible to change the country code after anpr_initialize has been called.
    While using the free trial license, you can try all languages.
    When you purchase a commercial license, you can only use the selected language.
  }
  initParams := 'text;country=' + countryCode;
  error := anpr_initialize(@tsanpr, initParams);
  if error <> '' then
  begin
    Writeln('anpr_initialize() failed (error=', error, ')');
    Exit(-1);
  end;

  imageDir := examplesBaseDir + '\img\' + countryCode + '\';

  // TODO: Try each function as needed
  anprFunc := readImageFile;
  // anprFunc := readEncodedImage;
  // anprFunc := readPixelBuffer;

  // TODO: Try each output format as needed
  outputFormat := 'text';
  // outputFormat := 'json';
  // outputFormat := 'yaml';
  // outputFormat := 'xml';
  // outputFormat := 'csv';

  anprFunc(imageDir + 'licensePlate.jpg', outputFormat, '');    // Single license plate recognition (default)
  anprFunc(imageDir + 'multiple.jpg', outputFormat, 'vm');      // Recognize multiple license plates attached to vehicles
  anprFunc(imageDir + 'multiple.jpg', outputFormat, 'vmb');     // Recognize multiple license plates including motorcycles
  anprFunc(imageDir + 'surround.jpg', outputFormat, 'vms');     // Recognize multiple license plates with surround detection
  anprFunc(imageDir + 'surround.jpg', outputFormat, 'dms');     // Recognize multiple surrounding objects (vehicles)
  anprFunc(imageDir + 'surround.jpg', outputFormat, 'dmsr');    // Recognize multiple surrounding objects and license plates

  // Recognize multiple surrounding objects and license plates within RoI
  anprFunc(imageDir + 'surround.jpg', outputFormat, 'dmsri549,700,549,2427,1289,2427,1289,700');

  Result := 0;
end;

begin
  SetConsoleOutputCP(65001);

  try
    // Load TSANPR engine
    if tsanpr_load(@tsanpr, PWideChar(getEngineFileName)) < 0 then
    begin
      Writeln('Failed to load TSANPR engine');
      Exit;
    end;

    // Try each country code as needed
    readLicensePlates('KR');
    // readLicensePlates('JP');
    // readLicensePlates('VN');

    tsanpr_unload;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

