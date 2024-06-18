/**
* anprCppLinux.cpp: 
*   dlsym 함수를 사용한 run-time binding 예제
*   이미지 파일 로딩은 opencv 이용
*
* 1. 엔진 파일 복사
*    사용하는 리눅스 플랫폼용 엔진 파일을 examples/bin/linux-`arch`/ 디렉토리에 압축 해제
*    tar xvpf tsanpr-*-linunx-`arch`.tar.xz
*
* 2. 빌드 환경 설치
*    Debian/Ubuntu:
*      sudo apt install -y build-essential g++ libopencv-dev python3-opencv
*    Oracle/RedHat:
*      sudo def install -y epel-release gcc gcc-c++ opencv opencv-devel
*
* 3. 빌드 및 실행
*    make
*    make run
*/


#include <unistd.h>
#include <sys/stat.h>
#include <dlfcn.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <opencv2/opencv.hpp>


#define IMG_PATH  "../../img/"		// 테스트용 이미지 디렉토리
#define LIB_NAME  "libtsanpr.so"


void* hlib = nullptr;

const char* (* anpr_initialize)(const char* mode) = nullptr; // [IN] 라이브러리 동작 방식 설정
const char* (* anpr_read_file)(
  const char* imgFileName,        // [IN] 입력 이미지 파일명
  const char* outputFormat,       // [IN] 출력 데이터 형식
  const char* options) = nullptr; // [IN] 기능 옵션
const char* (* anpr_read_pixels)(
  const unsigned char* pixels,    // [IN] 이미지 픽셀 시작 주소
  const unsigned long width,      // [IN] 이미지 가로 픽셀 수
  const unsigned long height,     // [IN] 이미지 세로 픽셀 수
  const unsigned long stride,     // [IN] 이미지 한 라인의 바이트 수
  const char* pixelFormat,        // [IN] 이미지 픽셀 형식 
  const char* outputFormat,       // [IN] 출력 데이터 형식
  const char* options) = nullptr;


void readFile(const char* imgfile, const char* outputFormat, const char *options)
{
  printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options);
	
	// 파일명을 인자로 전달
  char* result = (char*)anpr_read_file(
		imgfile, 			// 이미지 파일명
		outputFormat,	// 차번 인식 결과 출력 형식 
		options);			// 차번 인식 옵션
  printf("%s\n", result);
}

// 여기서 이미지 파일을 로드해서 메모리
void readPixels(const char* imgfile, const char* outputFormat, const char* options)
{
  printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options);

	using namespace cv;

	// OpenCV로 이미지 파일을 로드
	Mat img = imread(imgfile);
	if (img.empty()) {
    printf("Image load failed!\n");
    return;
  }
	
	// 이미지 버퍼를 인자로 전달
	char* result = (char*)anpr_read_pixels(
		(const unsigned char *)img.data,	// 이미지 버퍼 시작 주소
		img.cols,			// 가로 픽셀 수
		img.rows,			// 세로 픽셀 수
		0,						// 다음 라인까지의 바이트 수 (0이면 자동)
		"BGR",				// 로딩된 이미지 형식
		outputFormat,	// 차번 인식 결과 출력 형식
		options);			// 차번 인식 옵션
  printf("%s\n", result);
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

int getExecDirectory(char *buf, int buflen)
{
  int len = readlink("/proc/self/exe", buf, buflen - 1);
  if (len == -1) {
		perror("readlink");
		return -1;
	}
	
	buf[len] = '\0';
	char *base = basename(buf);
	if (!base) {
		perror("basename");
		return -2;
	}
	
	*base = '\0';
	return 0;
}
	

int loadEngineModule()
{
	// 실행 파일과 같은 디렉토리에 엔진 파일들이 있는 것으로 간주함
	char libPath[1024];
	int res = getExecDirectory(libPath, sizeof(libPath));
	if (res < 0)
		return res;
  
	strcat(libPath, LIB_NAME);
	hlib = dlopen(libPath, RTLD_NOW);
  if (!hlib) {
    printf("Cannot load module.\n");
    return -3;
  }

	
	anpr_initialize = (const char* (*)(const char*))dlsym(hlib, "anpr_initialize");
  if (!anpr_initialize) {
    printf("anpr_initialize() not found.\n");
    dlclose(hlib);
    hlib = nullptr;
    return -4;
  }

  anpr_read_file = (const char* (*)(const char*, const char*, const char*))dlsym(hlib, "anpr_read_file");
  if (!anpr_read_file) {
    printf("anpr_read_file() not found.\n");
    dlclose(hlib);
    hlib = nullptr;
    anpr_initialize = nullptr;
    return -5;
  }

  anpr_read_pixels = (const char* (*)(const unsigned char*, const unsigned long, const unsigned long, const unsigned long, const char*, const char*, const char*))dlsym(hlib, "anpr_read_pixels");
  if (!anpr_read_pixels) {
    printf("anpr_read_pixels() not found.\n");
    dlclose(hlib);
    hlib = nullptr;
    anpr_initialize = nullptr;
    anpr_read_file = nullptr;
    return -6;
  }

  return 0;
}

void unloadEngineModule()
{
  if (hlib) {
    dlclose(hlib);
    hlib = nullptr;
    anpr_initialize = nullptr;
    anpr_read_file = nullptr;
    anpr_read_pixels = nullptr;
  }
}

int main(int ac, char** av)
{
  int res = loadEngineModule();
  if (res < 0)
    return res;

  const char* error = anpr_initialize("text");
  if (error[0]) {
    printf("anpr_initialize() failed (error=%s)\n", error);
    unloadEngineModule();
    return -3;
  }

	// 이미지 파일 입력
  anprDemo1("text");
  anprDemo1("json");
  anprDemo1("yaml");
  anprDemo1("xml");
  anprDemo1("csv");

	// 이미지 메모리 버퍼 입력
  anprDemo2("text");
  anprDemo2("json");
  anprDemo2("yaml");
  anprDemo2("xml");
  anprDemo1("csv");

	
  unloadEngineModule();

  return 0;
}
