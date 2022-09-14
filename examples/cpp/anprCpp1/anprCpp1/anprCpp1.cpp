// anprCpp1.cpp : Link-time binding (Import library 이용)
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

// TS-ANPR
#include <tsanpr.h>
#pragma comment(lib, "tsanpr.lib")


#define IMG_PATH  "..\\..\\img\\"


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
  readFile(IMG_PATH "licensePlate.jpg", outputFormat, "v");
  readFile(IMG_PATH "licensePlate.jpg", outputFormat, "");
  readFile(IMG_PATH "multiple.jpg", outputFormat, "vm");
  readFile(IMG_PATH "multiple.jpg", outputFormat, "");
  readFile(IMG_PATH "surround.jpg", outputFormat, "vms");
  readFile(IMG_PATH "surround.jpg", outputFormat, "");
}

void anprDemo2(const char* outputFormat)
{
  readPixels(IMG_PATH "licensePlate.jpg", outputFormat, "v");
  readPixels(IMG_PATH "licensePlate.jpg", outputFormat, "");
  readPixels(IMG_PATH "multiple.jpg", outputFormat, "vm");
  readPixels(IMG_PATH "multiple.jpg", outputFormat, "");
  readPixels(IMG_PATH "surround.jpg", outputFormat, "vms");
  readPixels(IMG_PATH "surround.jpg", outputFormat, "");
}


int main(int ac, char** av)
{
  (void)_setmode(_fileno(stdout), _O_TEXT); //콘솔에 텍스트 출력
  SetConsoleOutputCP(65001);                // 콘솔에 한글 출력 모드

  const char *error = anpr_initialize("text");
  if (error) {
    printf("%s\n", error);
    return -1;
  }

  anprDemo1("text");
  anprDemo1("json");
  anprDemo1("yaml");
  anprDemo1("xml");

  // image file을 로딩하기 위해 사용함
  Gdiplus::GdiplusStartupInput gdiplusStartupInput;
  ULONG_PTR gdiplusToken;
  Gdiplus::GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);

  anprDemo2("text");
  anprDemo2("json");
  anprDemo2("yaml");
  anprDemo2("xml");

  Gdiplus::GdiplusShutdown(gdiplusToken);
  return 0;
}
