[English](../../) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# Zig 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Zig/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Zig/anpr)

### 1. 엔진 파일 복사

_**[참고]** 이 예제의 경우 다른 예제들과 엔진 파일을 공유하기 위해 examples/bin/ 디렉토리에 엔진 파일을 배치합니다. 실제 배포시는 일반적으로 응용 프로그램의 실행 파일이 있는 디렉토리에 엔진 파일을 복사합니다._

- 윈도우즈 x86 64비트
  엔진 파일을 `examples/bin/windows-x86_64` 디렉토리에 압축 해제
  ```sh
  7z x tsanpr*-windows-x86_64.7z
  ```
- 윈도우즈 x86 32비트
  엔진 파일을 `examples/bin/windows-x86` 디렉토리에 압축 해제
  ```sh
  7z x tsanpr*-windows-x86.7z
  ```
- 리눅스 x86 64비트
  엔진 파일을 `examples/bin/linux-x86_64` 디렉토리에 압축 해제
  ```sh
  tar xvf tsanpr-linux-x86_64.tar.xz
  ```
- 리눅스 arm 64비트
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
  └── Zig
      └── anpr                   # 소스 디렉토리
         ├── build.zig
         ├── src/
         │   ├── main.zig
         │   ├── tsanpr.c
         │   ├── tsanpr.h
         │   └── tsanpr.zig
         └── zig-out/
             └── bin/            # 출력 디렉토리
  ```

### 2. 사전 준비 사항

1. Zig 설치 (최신 안정 버전 권장)

   **Windows:**

   ```sh
   # https://ziglang.org/download/ 에서 다운로드
   # 또는 Scoop 사용
   scoop install zig
   ```

   **Linux:**

   ```sh
   # https://ziglang.org/download/ 에서 다운로드
   # 또는 패키지 매니저 사용 (배포판에 따라 다름)
   sudo snap install zig --classic --beta
   ```

2. 설치 확인

   ```sh
   zig version
   ```

### 3. 실행 방법

1. Zig 예제 디렉토리로 이동

   ```sh
   cd Zig/anpr
   ```

2. 예제 빌드 및 실행

   ```sh
   # 빌드 및 실행
   zig build run

   # 빌드만 수행
   zig build

   # 최적화하여 빌드
   zig build -Doptimize=ReleaseFast

   # 테스트 실행
   zig build test
   ```

### 4. 참고사항

- Zig 구현은 다른 언어 예제들과 동일한 기능을 제공합니다
- Zig의 C 상호운용성을 사용하여 네이티브 TSANPR 라이브러리와 인터페이스합니다
- Zig의 컴파일 타임 안전성과 성능은 시스템 프로그래밍에 탁월합니다
- Windows 및 Linux 크로스 플랫폼 지원
- 더 나은 배포 유연성을 위한 런타임 라이브러리 로딩

### 5. 기능

- **파일 기반 인식**: 이미지 파일을 직접 처리
- **인코딩된 이미지 처리**: 인코딩된 이미지 데이터 처리 (JPEG, PNG 등)
- **픽셀 버퍼 처리**: 원시 픽셀 데이터 처리 (간소화된 구현)
- **다양한 출력 형식**: 텍스트, JSON, YAML, XML, CSV 출력 지원
- **다양한 인식 모드**: 단일 번호판, 다중 번호판, 차량 감지 등
- **관심 영역 (RoI)**: 이미지 내 특정 영역만 처리
- **다국가 지원**: 다양한 번호판 형식 지원 (KR, JP, VN 등)

### 6. API 참조

#### TSANPR 함수

Zig 구현은 C interop를 통해 다음 함수들을 제공합니다:

**초기화:**

- `TSANPR_load(library_path)`: TSANPR 라이브러리 로드
- `anpr_initialize(mode)`: ANPR 엔진 초기화

**핵심 함수:**

- `anpr_read_file(img_file_name, output_format, options)`: 이미지 파일 처리
- `anpr_read_pixels(pixels, width, height, stride, pixel_format, output_format, options)`: 픽셀 데이터 처리

#### 인식 옵션

- `""`: 단일 번호판 인식 (기본값)
- `"vm"`: 차량에 부착된 다중 번호판 인식
- `"vmb"`: 차량에 부착된 다중 번호판 인식 (오토바이 포함)
- `"vms"`: 서라운드 감지를 통한 차량 다중 번호판 인식
- `"dms"`: 다중 서라운드 객체 인식 (차량)
- `"dmsr"`: 다중 서라운드 객체 (차량) 및 번호판 인식
- `"dmsri<좌표>"`: 관심 영역 내 인식

#### 출력 형식

- `"text"`: 일반 텍스트 출력
- `"json"`: JSON 형식 출력
- `"yaml"`: YAML 형식 출력
- `"xml"`: XML 형식 출력
- `"csv"`: CSV 형식 출력
