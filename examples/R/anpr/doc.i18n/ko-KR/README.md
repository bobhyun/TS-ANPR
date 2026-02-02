[English](../../) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# R 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/R/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/R/anpr)

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
  └── R
      └── anpr                   # 소스 디렉토리
         ├── anpr.R              # 메인 예제 스크립트
         ├── tsanpr.R            # TSANPR R6 래퍼 클래스
         ├── src/                # C 래퍼 소스
         │   ├── tsanpr_r.c
         │   ├── Makevars
         │   └── Makevars.win
         └── DESCRIPTION
  ```

### 2. 사전 요구 사항

1. R 및 시스템 종속성 설치

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get update
   # R 및 개발 도구 설치
   sudo apt-get install -y r-base r-base-dev
   # R 패키지용 시스템 라이브러리 설치
   sudo apt-get install -y libcurl4-openssl-dev libmagick++-dev
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   # CentOS/RHEL 8+ / Fedora
   sudo dnf install -y R R-devel
   sudo dnf install -y libcurl-devel ImageMagick-c++-devel

   # CentOS/RHEL 7
   sudo yum install -y R R-devel
   sudo yum install -y libcurl-devel ImageMagick-c++-devel
   ```

   **Windows:**

   - https://cran.r-project.org/bin/windows/base/ 에서 R 다운로드
   - https://cran.r-project.org/bin/windows/Rtools/ 에서 Rtools 설치

2. 필수 R 패키지 설치

   ```sh
   # R6 패키지 설치 (필수)
   sudo Rscript -e 'install.packages("R6", repos="https://cloud.r-project.org")'

   # magick 패키지 설치 (선택, readPixelBuffer용)
   sudo Rscript -e 'install.packages("magick", repos="https://cloud.r-project.org")'
   ```

   _참고: Windows에서는 `sudo` 없이 Rscript 명령을 실행하세요._

3. C 래퍼 컴파일

   **Linux/macOS:**

   ```sh
   cd examples/R/anpr
   R CMD SHLIB src/tsanpr_r.c -o src/tsanpr_r.so
   ```

   **Windows:**

   ```sh
   cd examples/R/anpr
   R CMD SHLIB src/tsanpr_r.c -o src/tsanpr_r.dll
   ```

### 3. 실행 방법

```sh
cd examples/R/anpr

# ANPR 예제 실행
Rscript anpr.R
```

**R 콘솔에서:**

```r
setwd("examples/R/anpr")
source("anpr.R")
```

**대화형 모드:**

```r
# TSANPR 래퍼 로드
source("tsanpr.R")

# TSANPR 초기화
engine_path <- "../../bin/linux-x86_64/libtsanpr.so"  # 플랫폼에 맞게 조정
tsanpr <- TSANPR$new(engine_path)

# 엔진 초기화
tsanpr$anpr_initialize("text;country=KR")

# 이미지 처리
result <- tsanpr$anpr_read_file("../../img/KR/licensePlate.jpg", "text", "")
print(result)
```

### 4. 기능

- **readImageFile**: `anpr_read_file()`을 사용하여 이미지 파일 직접 처리
- **readEncodedImage**: 'encoded' 형식으로 `anpr_read_pixels()`를 사용하여 인코딩된 이미지 바이트 처리
- **readPixelBuffer**: magick 패키지를 사용하여 원시 RGB 픽셀 데이터 처리
- **동적 로딩**: TSANPR 라이브러리는 C 래퍼를 통해 런타임에 로드됨
- **크로스 플랫폼**: Windows 및 Linux (x86_64, ARM64) 지원

### 5. API 참조

#### TSANPR 클래스

`TSANPR` R6 클래스는 다음 메서드를 제공합니다:

```r
TSANPR <- R6Class("TSANPR",
  public = list(
    initialize = function(library_path),
    anpr_initialize = function(mode),
    anpr_read_file = function(img_file_name, output_format, options),
    anpr_read_pixels = function(pixels, width, height, stride, pixel_format, output_format, options),
    is_loaded = function()
  )
)
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

이 예제는 R의 FFI와 TSANPR 라이브러리를 연결하기 위해 C 래퍼(`src/tsanpr_r.c`)를 사용합니다:

**동적 라이브러리 로딩:**
- **Linux**: libdl의 `dlopen()`, `dlsym()`, `dlclose()` 사용
- **Windows**: kernel32의 `LoadLibrary()`, `GetProcAddress()`, `FreeLibrary()` 사용

**R 통합:**
- 효율적인 데이터 교환을 위해 R의 `.Call()` 인터페이스 사용
- `R_registerRoutines()`를 통해 C 루틴 적절히 등록
- R6 클래스로 깔끔한 객체 지향 인터페이스 제공

R 코드는 컴파일된 C 래퍼를 로드하고, C 래퍼는 TSANPR 라이브러리 함수를 동적으로 로드하여 호출합니다.

### 7. 문제 해결

**컴파일 문제:**

- R 개발 패키지가 설치되어 있는지 확인 (Debian/Ubuntu에서 `r-base-dev`)
- Windows에서는 Rtools를 설치하고 PATH에 추가
- C 컴파일러 사용 가능 여부 확인: `R CMD config CC`

**라이브러리 로딩 문제:**

- 컴파일된 래퍼가 `src/` 디렉토리에 있는지 확인
- TSANPR 라이브러리 경로가 올바른지 확인
- Linux에서 필요시 `LD_LIBRARY_PATH` 설정

**패키지 종속성:**

- R6 패키지 설치: `install.packages("R6")`
- 픽셀 버퍼 처리용: `install.packages("magick")`
