{
  이 예제는 TS-ANPR 엔진 파일을 다운로드받아 examples/bin/ 디렉토리에
  압축을 풀어 아래와 같은 디렉토리 구조로 만들어진 상태에서 동작합니다.
  examples
    /bin
      /windows-x86_64
      /windows-x86
      /linux-x86_64
      /linux-aarch64

  컴파일된 실행파일과 작업 디렉토리는 /bin 아래 각 target platform별
  엔진 파일이 있는 디렉토리로 설정되어 있습니다.
}

program anprDelphi;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils, Windows, Vcl.Imaging.jpeg, Vcl.Graphics;

const
  IMG_PATH = '..\..\img\';

{
  const char* WINAPI anpr_initialize(const char* mode); // [IN] 라이브러리 동작 방식 설정
}
function anpr_initialize(mode: PUtf8Char): PUtf8Char;
  stdcall; external 'tsanpr.dll' name 'anpr_initialize';

{
  const char* WINAPI anpr_read_file(
    const char* imgFileName,      // [IN] 입력 이미지 파일명
    const char* outputFormat,     // [IN] 출력 데이터 형식
    const char* options);         // [IN] 기능 옵션
}
function anpr_read_file(imgFileName, outputFormat, options: PUtf8Char): PUtf8Char;
  stdcall; external 'tsanpr.dll' name 'anpr_read_file';

{
  const char* WINAPI anpr_read_pixels(
    const unsigned char* pixels,  // [IN] 이미지 픽셀 시작 주소
    const unsigned long width,    // [IN] 이미지 가로 픽셀 수
    const unsigned long height,   // [IN] 이미지 세로 픽셀 수
    const unsigned long stride,   // [IN] 이미지 한 라인의 바이트 수
    const char* pixelFormat,      // [IN] 이미지 픽셀 형식
    const char* outputFormat,     // [IN] 출력 데이터 형식
    const char* options);         // [IN] 기능 옵션
}
function anpr_read_pixels(pixels: Pointer; width, height, stride: Integer; pixelFormat, outputFormat, options: PUtf8Char): PUtf8Char;
  stdcall; external 'tsanpr.dll' name 'anpr_read_pixels';


procedure readFile(imgfile, outputFormat, options: string);
var
  result: PUtf8Char;
begin
  Write(Format('%s (outputFormat="%s", options="%s") => ', [imgfile, outputFormat, options]));

  // 이미지 파일 입력으로 차번 인식
  result := anpr_read_file(PUtf8Char(UTF8Encode(imgfile)), PUtf8Char(UTF8Encode(outputFormat)), PUtf8Char(UTF8Encode(options)));
  WriteLn(UTF8ToString(result));
end;


procedure readPixels(imgfile, outputFormat, options: string);
var
  result: PUtf8Char;
  jpg: TJPEGImage;
  bmp: TBitmap;
  pixelBuffer: PByteArray;
  imageStartAddress: Pointer;
  stride: Integer;

begin
  Write(Format('%s (outputFormat="%s", options="%s") => ', [imgfile, outputFormat, options]));

  // 이미지 파일을 메모리에 로딩
  jpg := TJPEGImage.Create;
  jpg.LoadFromFile(imgfile);

  bmp := TBitmap.Create;
  bmp.Assign(jpg);

  {
     [참고]
     bmp 이미지 메모리 구조는 그림이 위 아래가 뒤집어져 있는 모양이므로
     메모리 끝 쪽에서부터 올라오면서 스캔함

      메모리버퍼 시작 주소 -> +-------------------------------+
                          |                               |
                       ^  |                               |
                       |  |                               |
                       |  |                               |
                       |  |                               |
              스캔 방향 |  |                               |
                          |                               |
    이미지 첫라인 시작 주소 -> +-------------------------------+
  }

  // 이미지 첫라인 시작 주소 얻기
  pixelBuffer := bmp.ScanLine[bmp.Height - 1];
  imageStartAddress := @pixelBuffer[0];

  // 다음 라인까지의 거리(byte 수) 구하기 (bmp 이미지 메모리 구조는 역방향 스캔이므로 stride가 음수임)
  stride := Integer(bmp.ScanLine[bmp.Height - 2]) - Integer(PixelBuffer);

  result := anpr_read_pixels(imageStartAddress, bmp.Width, bmp.Height, stride, PUtf8Char(UTF8Encode('BGR')), PUtf8Char(UTF8Encode(outputFormat)), PUtf8Char(UTF8Encode(options)));
  WriteLn(UTF8ToString(result));

  bmp.Free;
  jpg.Free;
end;


procedure anprDemo1(outputFormat: string);
begin
	// anpr
  readFile(IMG_PATH + 'licensePlate.jpg', outputFormat, 'v');
  readFile(IMG_PATH + 'licensePlate.jpg', outputFormat, '');
  readFile(IMG_PATH + 'multiple.jpg', outputFormat, 'vm');
  readFile(IMG_PATH + 'multiple.jpg', outputFormat, '');
  readFile(IMG_PATH + 'surround.jpg', outputFormat, 'vms');
  readFile(IMG_PATH + 'surround.jpg', outputFormat, '');

	// object detection
	readFile(IMG_PATH + 'surround.jpg', outputFormat, 'dms');
	readFile(IMG_PATH + 'surround.jpg', outputFormat, 'dmsr');
end;

procedure anprDemo2(outputFormat: string);
begin
	// anpr
  readPixels(IMG_PATH + 'licensePlate.jpg', outputFormat, 'v');
  readPixels(IMG_PATH + 'licensePlate.jpg', outputFormat, '');
  readPixels(IMG_PATH + 'multiple.jpg', outputFormat, 'vm');
  readPixels(IMG_PATH + 'multiple.jpg', outputFormat, '');
  readPixels(IMG_PATH + 'surround.jpg', outputFormat, 'vms');
  readPixels(IMG_PATH + 'surround.jpg', outputFormat, '');
	
	// object detection
	readPixels(IMG_PATH + 'surround.jpg', outputFormat, 'dms');
	readPixels(IMG_PATH + 'surround.jpg', outputFormat, 'dmsr');
end;

var
  error: PUtf8Char;
begin
  // 출력 코드 페이지를 UTF-8로 설정
  SetConsoleOutputCP(65001);

  try
    error := anpr_initialize(PUtf8Char(Utf8String('text')));
    if error <> '' then
    begin
      WriteLn(UTF8ToString(error));
      Exit()
    end
    else
    begin
      // 이미지 파일을 입력으로 사용하는 예제
      anprDemo1('text');
      anprDemo1('json');
      anprDemo1('yaml');
      anprDemo1('xml');
      anprDemo1('csv');

      // 픽셀 버퍼를 입력으로 사용하는 예제
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
