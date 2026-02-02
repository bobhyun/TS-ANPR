[English](../../) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# V 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/V/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/V/anpr)

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
  ```
  examples/
  ├── bin/
  │   ├── windows-x86_64/       # Windows (x86_64)용 엔진
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── windows-x86/          # Windows (x86)용 엔진
  │   │   └── ...
  │   ├── linux-x86_64/         # Linux (x86_64)용 엔진
  │   │   └── ...
  │   └── linux-aarch64/        # Linux (arm64)용 엔진
  │       └── ...
  ├── img/
  └── V/
      └── anpr/                 # V 모듈 디렉토리
          ├── v.mod             # 모듈 정의
          ├── anpr.v            # 메인 예제
          └── tsanpr.v          # TSANPR 래퍼 모듈
  ```

### 2. 사전 요구사항

1. V 설치 (최신 버전 권장)

   **Windows:**

   ```sh
   # Scoop 사용
   scoop install vlang

   # 또는 미리 빌드된 바이너리 다운로드:
   # https://github.com/vlang/v/releases
   ```

   **Linux:**

   ```sh
   # 소스에서 클론하여 빌드
   git clone https://github.com/vlang/v
   cd v
   make
   sudo ./v symlink
   ```

2. 설치 확인

   ```sh
   v version
   ```

### 3. 실행 방법

```sh
cd examples/V/anpr

# ANPR 예제 실행
v run .
```

**다른 방법:**

```sh
# 컴파일 후 별도로 실행
v .
./anpr        # Linux
anpr.exe      # Windows

# 최적화와 함께 컴파일
v -prod .
```

### 4. 기능

- **readImageFile**: `anpr_read_file()`을 사용하여 이미지 파일 직접 처리
- **readEncodedImage**: 'encoded' 형식으로 `anpr_read_pixels()`를 사용하여 인코딩된 이미지 바이트 처리
- **readPixelBuffer**: V의 `stbi` 모듈(stb_image)을 사용하여 원시 RGB 픽셀 데이터 처리
- **동적 로딩**: TSANPR 라이브러리는 V의 `dl` 모듈을 통해 런타임에 로드됨
- **크로스 플랫폼**: Windows 및 Linux (x86_64, x86, ARM64) 지원

### 5. API 참조

#### TSANPR 모듈

`tsanpr` 모듈은 다음을 제공합니다:

```v
// TSANPR 인스턴스 생성
pub fn new(library_path string) !TSANPR

// TSANPR 메서드
pub fn (mut t TSANPR) destroy()
pub fn (t &TSANPR) initialize(mode string) string
pub fn (t &TSANPR) read_file(img_file_name string, output_format string, options string) string
pub fn (t &TSANPR) read_pixels(pixels []u8, width u64, height u64, stride i64, pixel_format string, output_format string, options string) string
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

이 예제는 동적 라이브러리 로딩을 위해 V의 `dl` 모듈을 사용합니다:

**동적 라이브러리 로딩:**
- **Linux**: V의 `dl` 모듈을 통해 `dlopen()`, `dlsym()`, `dlclose()` 사용
- **Windows**: V의 `dl` 모듈을 통해 `LoadLibrary()`, `GetProcAddress()`, `FreeLibrary()` 사용

**V 통합:**
- V의 C 인터롭으로 네이티브 라이브러리에 직접 함수 포인터 호출 가능
- V 문자열과 C 문자열 간 자동 변환
- `defer`를 사용한 메모리 안전한 정리

**픽셀 버퍼 처리:**
`readPixelBuffer` 함수는 이미지 디코딩을 위해 V의 내장 `stbi` 모듈(stb_image 래퍼)을 사용합니다. 이미지를 로드하고 원시 RGB 픽셀 데이터를 추출하여 적절한 픽셀 형식으로 `anpr_read_pixels()`에 전달합니다.

### 7. 문제 해결

**컴파일 문제:**

- V가 올바르게 설치되어 PATH에 있는지 확인
- V 버전 확인: `v version`
- 필요시 V 업데이트: `v up`

**라이브러리 로딩 문제:**

- TSANPR 라이브러리가 예상 위치에 있는지 확인
- Linux에서 필요시 `LD_LIBRARY_PATH` 확인
- 라이브러리 아키텍처가 V 빌드와 일치하는지 확인 (64비트 vs 32비트)

**런타임 문제:**

- 엔진 파일(`.eon`)이 라이브러리와 같은 디렉토리에 있는지 확인
- `tshelper`를 사용하여 라이선스가 설치되어 있는지 확인
