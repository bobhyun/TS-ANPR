program anprDelphi;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, Windows, Vcl.Imaging.jpeg, Vcl.Graphics;

const
  IMG_PATH = '..\..\..\img\';

function anpr_initialize(outputFormat: PAnsiChar): PAnsiChar;
  stdcall; external 'tsanpr.dll' name 'anpr_initialize';
function anpr_read_file(imgFileName, outputFormat, options: PAnsiChar): PAnsiChar;
  stdcall; external 'tsanpr.dll' name 'anpr_read_file';
function anpr_read_pixels(pixels: Pointer; width, height, stride: Integer; pixelFormat, outputFormat, options: PAnsiChar): PAnsiChar;
  stdcall; external 'tsanpr.dll' name 'anpr_read_pixels';

procedure readFile(imgfile, outputFormat, options: string);
var
  result: string;
begin
  Write(Format('%s (outputFormat="%s", options="%s") => ', [imgfile, outputFormat, options]));
  result := anpr_read_file(PAnsiChar(Utf8String(imgfile)), PAnsiChar(Utf8String(outputFormat)), PAnsiChar(Utf8String(options)));
  WriteLn(PWideChar(result));
end;

procedure readPixels(imgfile, outputFormat, options: string);
var
  result: string;
  jpg: TJPEGImage;
  bmp: TBitmap;
  pxlbuf :^TRGBTriple;
  buflen: Integer;
begin
  Write(Format('%s (outputFormat="%s", options="%s") => ', [imgfile, outputFormat, options]));

  jpg := TJPEGImage.Create;
  jpg.LoadFromFile(imgfile);

  bmp := TBitmap.Create;
  bmp.Assign(jpg);

  {
    [알림]
    제가 Delphi 프로그래밍에 익숙하지 못해
    로딩된 이미지의 픽셀 버퍼 주소를 직접 얻는 방법을 몰라서
    pxlbuf에 원본 이미지를 복사해 낸 다음
    anpr_read_pixels(pxlbuf, ...)로 전달하는 방법을 사용해서 느리게 동작합니다.

    더 효율적인 방법을 알고 계시면
    아래 저희 프로젝트 게시판에 글을 남겨주시면 고맙겠습니다. ^^;
    https://github.com/bobhyun/TS-ANPR/issues/new
  }
  buflen := bmp.Width * bmp.Height * SizeOf (TRGBTriple);
  GetMem (pxlbuf, buflen);
  GetBitmapBits (bmp.Handle, buflen, pxlbuf);

  result := anpr_read_pixels(pxlbuf, bmp.Width, bmp.Height, 0, PAnsiChar(Utf8String('BGR')), PAnsiChar(Utf8String(outputFormat)), PAnsiChar(Utf8String(options)));
  WriteLn(PWideChar(result));

  FreeMem(pxlbuf);
  bmp.Free;
  jpg.Free;
end;


procedure anprDemo1(outputFormat: string);
begin
  readFile(IMG_PATH + 'licensePlate.jpg', outputFormat, 'v');
  readFile(IMG_PATH + 'licensePlate.jpg', outputFormat, '');
  readFile(IMG_PATH + 'multiple.jpg', outputFormat, 'vm');
  readFile(IMG_PATH + 'multiple.jpg', outputFormat, '');
  readFile(IMG_PATH + 'surround.jpg', outputFormat, 'vms');
  readFile(IMG_PATH + 'surround.jpg', outputFormat, '');
end;

procedure anprDemo2(outputFormat: string);
begin
  readPixels(IMG_PATH + 'licensePlate.jpg', outputFormat, 'v');
  readPixels(IMG_PATH + 'licensePlate.jpg', outputFormat, '');
  readPixels(IMG_PATH + 'multiple.jpg', outputFormat, 'vm');
  readPixels(IMG_PATH + 'multiple.jpg', outputFormat, '');
  readPixels(IMG_PATH + 'surround.jpg', outputFormat, 'vms');
  readPixels(IMG_PATH + 'surround.jpg', outputFormat, '');
end;

var
  error: string;
begin
  try
    error := anpr_initialize(PAnsiChar(Utf8String('text')));
    if error <> '' then
    begin
      WriteLn(error);
      Exit()
    end
    else
    begin

      anprDemo1('text');
      anprDemo1('json');
      anprDemo1('yaml');
      anprDemo1('xml');
      anprDemo1('csv');

      anprDemo2('text');
      anprDemo2('json');
      anprDemo2('yaml');
      anprDemo2('xml');
      anprDemo2('csv');

    end

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
