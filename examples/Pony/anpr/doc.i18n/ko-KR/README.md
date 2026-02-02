[English](../../) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# Pony 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Pony/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Pony/anpr)

### 1. 엔진 파일 복사

_**[참고]** 이 예제에서는 다른 예제와 공유하기 위해 엔진 파일을 examples/bin/ 디렉토리에 압축 해제합니다. 그러나 실제 배포 시에는 일반적으로 애플리케이션 실행 파일이 있는 디렉토리에 엔진 파일을 복사합니다._

- Windows x86 64비트의 경우
  엔진 파일을 `examples/bin/windows-x86_64` 디렉토리에 압축 해제합니다
  ```sh
  7z x tsanpr*-windows-x86_64.7z
  ```
- Windows x86 32비트의 경우
  엔진 파일을 `examples/bin/windows-x86` 디렉토리에 압축 해제합니다
  ```sh
  7z x tsanpr*-windows-x86.7z
  ```
- Linux x86 64비트의 경우
  엔진 파일을 `examples/bin/linux-x86_64` 디렉토리에 압축 해제합니다
  ```sh
  tar xvf tsanpr-linux-x86_64.tar.xz
  ```
- Linux arm 64비트의 경우
  엔진 파일을 `examples/bin/linux-aarch64` 디렉토리에 압축 해제합니다
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
  └── Pony
      └── anpr                   # 프로젝트 디렉토리
         ├── src/                # 메인 패키지
         │   ├── main.pony
         │   └── tsanpr.pony
         ├── ffi/                # C FFI 래퍼
         │   ├── tsanpr_wrapper.c
         │   ├── tsanpr_wrapper.h
         │   ├── image_loader.c
         │   └── stb_image.h
         └── Makefile
  ```

### 2. 사전 요구 사항

1. Pony 컴파일러 설치

   **Ubuntu/Debian:**

   ```sh
   # ponyup 사용 (권장)
   curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/ponylang/ponyup/latest-release/ponyup-init.sh | sh
   source ~/.profile
   ponyup update ponyc release
   ```

   **macOS:**

   ```sh
   brew install ponyc
   ```

   **Windows:**

   ```sh
   # Chocolatey 사용
   choco install ponyc

   # 또는 https://github.com/ponylang/ponyc/releases 에서 다운로드
   ```

2. C 컴파일러 설치 (GCC 또는 Clang)

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get install build-essential
   ```

   **macOS:**

   ```sh
   xcode-select --install
   ```

   **Windows:**

   - Visual Studio Build Tools 또는 MinGW-w64 설치

3. 설치 확인

   ```sh
   ponyc --version
   gcc --version
   ```

### 3. 실행 방법

```sh
cd examples/Pony/anpr

# 빌드 및 실행
make run

# 빌드만
make

# 릴리스 버전 빌드
make release

# 빌드 결과물 정리
make clean
```

### 4. 기능

- **readImageFile**: `anpr_read_file()`을 사용하여 이미지 파일 직접 처리
- **readEncodedImage**: 'encoded' 형식으로 `anpr_read_pixels()`를 사용하여 인코딩된 이미지 바이트 처리
- **readPixelBuffer**: stb_image 라이브러리를 사용하여 원시 RGB 픽셀 데이터 처리
- **동적 로딩**: TSANPR 라이브러리는 dlopen/LoadLibrary를 사용하여 런타임에 로드됨
- **크로스 플랫폼**: Windows 및 Linux (x86_64, ARM64) 지원

### 5. API 참조

#### TSANPR 클래스

```pony
class TSANPR
  new create(library_path: String) ?
  fun ref initialize(mode: String): String
  fun ref read_file(img_file_name: String, output_format: String, options: String): String
  fun ref read_pixels(pixels: Array[U8] val, width: U64, height: U64, stride: I64,
                     pixel_format: String, output_format: String, options: String): String
```

#### 인식 옵션

| 옵션 | 설명 |
|------|------|
| `""` | 단일 번호판 인식 (기본값) |
| `"vm"` | 차량에 부착된 여러 번호판 인식 |
| `"vmb"` | 여러 번호판 인식 (오토바이 포함) |
| `"vms"` | 서라운드 감지와 함께 인식 |
| `"dms"` | 여러 주변 객체 (차량) 감지 |
| `"dmsr"` | 객체 감지 및 번호판 인식 |
| `"dmsri<coords>"` | 관심 영역 내 인식 |

#### 출력 형식

`"text"`, `"json"`, `"yaml"`, `"xml"`, `"csv"`

### 6. 구현 참고 사항

이 예제는 동적 라이브러리 로딩과 이미지 처리를 위해 C 래퍼를 사용합니다:

**동적 라이브러리 로딩 (`tsanpr_wrapper.c`):**
- **Linux**: libdl의 `dlopen()`, `dlsym()`, `dlclose()` 사용
- **Windows**: kernel32의 `LoadLibrary()`, `GetProcAddress()`, `FreeLibrary()` 사용

**이미지 처리 (`image_loader.c`):**
- 이미지 디코딩을 위해 [stb_image](https://github.com/nothings/stb) 단일 헤더 라이브러리 사용
- 이미지를 로드하고 ANPR을 직접 호출하는 편의 함수 `image_anpr_read()` 제공

Pony 코드는 FFI를 통해 C 래퍼 함수를 호출하고, C 래퍼 함수는 동적으로 로드된 TSANPR 라이브러리 함수를 호출합니다.
