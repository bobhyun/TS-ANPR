[English](../../README.md) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# Fortran 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Fortran/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Fortran/anpr)

### 1. 엔진 파일 복사

_**[참고]** 이 예제에서는 다른 예제들과 공유하기 위해 엔진 파일을 examples/bin/ 디렉토리에 압축 해제합니다. 하지만 실제 배포 시에는 일반적으로 애플리케이션의 실행 파일이 위치한 디렉토리에 엔진 파일을 복사합니다._

- Windows x86 64비트용
  엔진 파일을 `examples/bin/windows-x86_64` 디렉토리에 압축 해제
  ```sh
  7z x tsanpr*-windows-x86_64.7z
  ```
- Windows x86 32비트용
  엔진 파일을 `examples/bin/windows-x86` 디렉토리에 압축 해제
  ```sh
  7z x tsanpr*-windows-x86.7z
  ```
- Linux x86 64비트용
  엔진 파일을 `examples/bin/linux-x86_64` 디렉토리에 압축 해제
  ```sh
  tar xvf tsanpr-linux-x86_64.tar.xz
  ```
- Linux arm 64비트용
  엔진 파일을 `examples/bin/linux-aarch64` 디렉토리에 압축 해제
  ```sh
  tar xvf tsanpr-linux-aarch64.tar.xz
  ```
- 디렉토리 구조
  ```sh
  examples
  ├── bin
  │   ├─── windows-x86_64        # Windows (x86_64)용 엔진 디렉토리
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86           # Windows (x86)용 엔진 디렉토리
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64           # Linux (x86_64)용 엔진 디렉토리
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # Linux (arm64)용 엔진 디렉토리
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # 이미지 디렉토리
  └── Fortran
      └── anpr                   # 프로젝트 디렉토리
         ├── src                 # 소스 디렉토리
         │   ├── anpr.f90
         │   └── tsanpr_module.f90
         ├── build               # 빌드 출력 디렉토리 (make로 생성됨)
         └── Makefile
  ```

### 2. 사전 요구사항

1. Fortran 컴파일러 및 빌드 도구 설치

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get update
   sudo apt-get install gfortran build-essential
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   # CentOS/RHEL 7
   sudo yum install gcc-gfortran make

   # CentOS/RHEL 8+ / Fedora
   sudo dnf install gcc-gfortran make
   ```

   **Windows (MinGW/MSYS2):**

   ```sh
   # MSYS2를 먼저 설치한 후:
   pacman -S mingw-w64-x86_64-gcc-fortran mingw-w64-x86_64-make
   ```

2. 설치 확인

   ```sh
   gfortran --version
   make --version
   ```

### 3. 빌드 방법

1. Fortran 예제 디렉토리로 이동

   ```sh
   cd Fortran/anpr
   ```

2. 예제 빌드

   ```sh
   make all
   ```

3. 빌드 결과물 정리 (필요시)

   ```sh
   make clean
   ```

### 4. 실행 방법

1. `anpr` 예제 실행

   ```sh
   ./build/anpr
   ```

   또는 Windows에서:

   ```sh
   build/anpr.exe
   ```

### 5. 참고사항

- 이 Fortran 구현은 다른 언어 예제들과 동일한 기능을 제공합니다
- C 상호 운용성을 위해 현대적인 Fortran 2008 표준과 ISO C 바인딩을 사용합니다
- 동적 라이브러리 로딩은 시스템별 API를 통해 처리됩니다 (Unix의 dlopen, Windows의 LoadLibrary)

- 픽셀 버퍼 처리는 이 예제에서 단순화되었습니다 - 완전한 구현을 위해서는 이미지 처리 라이브러리와의 통합이 필요합니다
- 전처리기 지시문을 통해 크로스 플랫폼 컴파일을 지원합니다

### 6. 기능

- **파일 기반 인식**: 이미지 파일을 직접 처리
- **인코딩된 이미지 처리**: 인코딩된 이미지 데이터 처리 (JPEG, PNG 등)
- **픽셀 버퍼 처리**: 원시 픽셀 데이터 처리 (단순화된 구현)
- **다중 출력 형식**: 텍스트, JSON, YAML, XML, CSV 출력 지원
- **다중 인식 모드**: 단일 번호판, 다중 번호판, 차량 감지 등
- **관심 영역(RoI)**: 이미지 내 특정 영역 처리
- **다국가 지원**: 다양한 번호판 형식 지원 (KR, JP, VN 등)
- **크로스 플랫폼 호환성**: Windows, Linux 지원

### 7. API 참조

#### TSANPR 모듈

`tsanpr_module`은 다음 타입과 프로시저를 제공합니다:

**타입:**

- `tsanpr_handle`: TSANPR 라이브러리 인스턴스 핸들

**초기화:**

- `tsanpr_init(tsanpr, library_path, status)`: 라이브러리 경로로 TSANPR 초기화
- `tsanpr_cleanup(tsanpr)`: TSANPR 리소스 정리

**핵심 함수:**

- `tsanpr_initialize(tsanpr, mode, error_msg, status)`: ANPR 엔진 초기화
- `tsanpr_read_file(tsanpr, img_file_name, output_format, options, result, status)`: 이미지 파일 처리
- `tsanpr_read_pixels(tsanpr, pixels, width, height, stride, pixel_format, output_format, options, result, status)`: 픽셀 데이터 처리

#### 인식 옵션

- `""`: 단일 번호판 인식 (기본값)
- `"vm"`: 차량에 부착된 다중 번호판 인식
- `"vmb"`: 차량에 부착된 다중 번호판 인식 (오토바이 포함)
- `"vms"`: 주변 감지와 함께 차량에 부착된 다중 번호판 인식
- `"dms"`: 다중 주변 객체 (차량) 인식
- `"dmsr"`: 다중 주변 객체 (차량)와 번호판 인식
- `"dmsri<좌표>"`: 관심 영역 내에서 인식

#### 출력 형식

- `"text"`: 일반 텍스트 출력
- `"json"`: JSON 형식 출력
- `"yaml"`: YAML 형식 출력
- `"xml"`: XML 형식 출력
- `"csv"`: CSV 형식 출력
