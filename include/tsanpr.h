/**
 * @file tsanpr.h
 * @author Bob Hyun (bobhyun@gmail.com)
 * @brief 
 * @version 1.4.0
 * @date 2022-07-31
 * 
 * @copyright Copyright (c) 2022 TS-Solution Co.,Ltd.
 * 
 */
#ifndef __TSANPR_H__
#define __TSANPR_H__

#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#define TS_ANPR_ENTRY __declspec(dllexport) const char* WINAPI
#else
#define TS_ANPR_ENTRY const char* 
#endif

// 라이브러리 초기화
TS_ANPR_ENTRY anpr_initialize(const char* mode); // [IN] 라이브러리 동작 방식 설정

// 이미지 파일에서 번호인식
TS_ANPR_ENTRY anpr_read_file(
  const char* imgFileName,      // [IN] 입력 이미지 파일명
  const char* outputFormat,     // [IN] 출력 데이터 형식
  const char* options);         // [IN] 기능 옵션

// 이미지 메모리 버퍼에서 번호인식
TS_ANPR_ENTRY anpr_read_pixels(
  const unsigned char* pixels,  // [IN] 이미지 픽셀 시작 주소
  const unsigned long width,    // [IN] 이미지 가로 픽셀 수
  const unsigned long height,   // [IN] 이미지 세로 픽셀 수
  const unsigned long stride,   // [IN] 이미지 한 라인의 바이트 수
  const char* pixelFormat,      // [IN] 이미지 픽셀 형식 
  const char* outputFormat,     // [IN] 출력 데이터 형식
  const char* options);         // [IN] 기능 옵션

#ifdef __cplusplus
};
#endif

#endif  // __TSANPR_H__
