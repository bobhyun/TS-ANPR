// anprCpp2.cpp: Run-time binding (LoadLibrary() 이용)
//

#define _CRT_SECURE_NO_WARNINGS
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <io.h>
#include <fcntl.h>
#include <fstream>

// GDI+
#include <objbase.h>
#include <oleauto.h>
#include <gdiplus.h>
#pragma comment( lib, "ole32.lib" )
#pragma comment( lib, "oleaut32.lib" )
#pragma comment(lib, "gdiplus.lib")


#define IMG_PATH  "..\\..\\..\\img\\"

HMODULE hdll;
const char* (WINAPI* anpr_initialize)(const char* mode); // [IN] 라이브러리 동작 방식 설정
const char* (WINAPI* anpr_read_file)(
  const char* imgFileName,      // [IN] 입력 이미지 파일명
  const char* outputFormat,     // [IN] 출력 데이터 형식
  const char* options);         // [IN] 기능 옵션
const char* (WINAPI* anpr_read_pixels)(
  const unsigned char* pixels,  // [IN] 이미지 픽셀 시작 주소
  const unsigned long width,    // [IN] 이미지 가로 픽셀 수
  const unsigned long height,   // [IN] 이미지 세로 픽셀 수
  const unsigned long stride,   // [IN] 이미지 한 라인의 바이트 수
  const char* pixelFormat,      // [IN] 이미지 픽셀 형식 
  const char* outputFormat,     // [IN] 출력 데이터 형식
  const char* options);


void readFile(const char* imgfile, const char* outputFormat, const char *options)
{
  printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options);
  char* result = (char*)anpr_read_file(imgfile, outputFormat, options);
  printf("%s\n", result);
}

wchar_t* utf8toUnicode(wchar_t* buf, int charLen, const char* src)
{
  int slen = (int)strlen(src);
  if (slen <= 0) {
    buf[0] = '\0';
    return buf;
  }

  int wlen = MultiByteToWideChar(CP_UTF8, 0, src, slen, buf, charLen);
  buf[wlen] = '\0';
  return buf;
}

void readPixels(const char* imgfile, const char* outputFormat, const char* options)
{
  printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options);
    
  wchar_t imgfileW[MAX_PATH];
  utf8toUnicode(imgfileW, _countof(imgfileW), imgfile);

  Gdiplus::Bitmap bitmap(imgfileW, false);
  int width = bitmap.GetWidth();
  int height = bitmap.GetHeight();
  Gdiplus::Rect rect = { 0, 0, width, height };
  Gdiplus::BitmapData img;
  bitmap.LockBits(&rect, Gdiplus::ImageLockMode::ImageLockModeRead, bitmap.GetPixelFormat(), &img);

  char* result = (char*)anpr_read_pixels((const unsigned char *)img.Scan0, img.Width, img.Height, img.Stride, "BGR", outputFormat, options);
  printf("%s\n", result);

  bitmap.UnlockBits(&img);
}

void anprDemo1(const char* outputFormat)
{
  // anpr
  readFile(IMG_PATH "licensePlate.jpg", outputFormat, "v");
  readFile(IMG_PATH "licensePlate.jpg", outputFormat, "");
  readFile(IMG_PATH "multiple.jpg", outputFormat, "vm");
  readFile(IMG_PATH "multiple.jpg", outputFormat, "");
  readFile(IMG_PATH "surround.jpg", outputFormat, "vms");
  readFile(IMG_PATH "surround.jpg", outputFormat, "");
	
	// object detection
	readFile(IMG_PATH "surround.jpg", outputFormat, "dms");
	readFile(IMG_PATH "surround.jpg", outputFormat, "dmsr");
}

void anprDemo2(const char* outputFormat)
{
  // anpr
  readPixels(IMG_PATH "licensePlate.jpg", outputFormat, "v");
  readPixels(IMG_PATH "licensePlate.jpg", outputFormat, "");
  readPixels(IMG_PATH "multiple.jpg", outputFormat, "vm");
  readPixels(IMG_PATH "multiple.jpg", outputFormat, "");
  readPixels(IMG_PATH "surround.jpg", outputFormat, "vms");
  readPixels(IMG_PATH "surround.jpg", outputFormat, "");
	
	// object detection
	readPixels(IMG_PATH "surround.jpg", outputFormat, "dms");
	readPixels(IMG_PATH "surround.jpg", outputFormat, "dmsr");
}

int loadEngineModule()
{
  hdll = LoadLibrary(L"tsanpr.dll");
  if (!hdll) {
    printf("Cannot load module.\n");
    return -1;
  }

  anpr_initialize = (const char* (WINAPI*)(const char*))GetProcAddress(hdll, "anpr_initialize");
  if (!anpr_initialize) {
    printf("anpr_initialize() not found.\n");
    FreeLibrary(hdll);
    hdll = NULL;
    return -2;
  }

  anpr_read_file = (const char* (WINAPI*)(const char*, const char*, const char*))GetProcAddress(hdll, "anpr_read_file");
  if (!anpr_read_file) {
    printf("anpr_read_file() not found.\n");
    FreeLibrary(hdll);
    hdll = NULL;
    anpr_initialize = NULL;
    return -4;
  }

  anpr_read_pixels = (const char* (WINAPI*)(const unsigned char*, const unsigned long, const unsigned long, const unsigned long, const char*, const char*, const char*))GetProcAddress(hdll, "anpr_read_pixels");
  if (!anpr_read_pixels) {
    printf("anpr_read_pixels() not found.\n");
    FreeLibrary(hdll);
    hdll = NULL; 
    anpr_initialize = NULL;
    anpr_read_file = NULL;
    return -5;
  }

  return 0;
}

void unloadEngineModule()
{
  if (hdll) {
    FreeLibrary(hdll);
    hdll = NULL;
    anpr_initialize = NULL;
    anpr_read_file = NULL;
    anpr_read_pixels = NULL;
  }
}

int main(int ac, char** av)
{
  (void)_setmode(_fileno(stdout), _O_TEXT); //콘솔에 텍스트 출력
  SetConsoleOutputCP(65001);                // 콘솔에 한글 출력 모드

  int res = loadEngineModule();
  if (res < 0)
    return res;

  const char* error = anpr_initialize("text");
  if (error[0]) {
    printf("anpr_initialize() failed (error=%s)\n", error);
    unloadEngineModule();
    return -3;
  }


  anprDemo1("text");
  anprDemo1("json");
  anprDemo1("yaml");
  anprDemo1("xml");
  anprDemo1("csv");

  // image file을 로딩하기 위해 사용함
  Gdiplus::GdiplusStartupInput gdiplusStartupInput;
  ULONG_PTR gdiplusToken;
  Gdiplus::GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);

  anprDemo2("text");
  anprDemo2("json");
  anprDemo2("yaml");
  anprDemo2("xml");
  anprDemo1("csv");

  Gdiplus::GdiplusShutdown(gdiplusToken);

  unloadEngineModule();

  return 0;
}
