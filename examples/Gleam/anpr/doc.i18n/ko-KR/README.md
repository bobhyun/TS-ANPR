[English](../../README.md) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# Gleam 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Gleam/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Gleam/anpr)

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
  │   │   ├── tsanpr-*.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86           # Windows (x86)용 엔진 디렉토리
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-*.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64           # Linux (x86_64)용 엔진 디렉토리
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-*.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # Linux (arm64)용 엔진 디렉토리
  │       ├── libtsanpr.so
  │       ├── tsanpr-*.eon
  │       └── tshelper
  ├── img                        # 이미지 디렉토리
  └── Gleam
      └── anpr                   # 프로젝트 디렉토리
         ├── c_src               # C NIF 소스 파일
         │   ├── tsanpr_nif.c    # NIF 구현
         │   └── stb_image.h     # 이미지 디코딩 라이브러리
         ├── src                 # Gleam/Erlang 소스 파일
         │   ├── anpr.gleam      # 메인 모듈
         │   ├── tsanpr.gleam    # TSANPR API 모듈
         │   ├── tsanpr_ffi.erl  # NIF 래퍼 모듈
         │   └── anpr_ffi.erl    # 헬퍼 함수
         ├── priv                # 컴파일된 NIF 라이브러리 (생성됨)
         │   └── tsanpr_nif.dll/.so
         ├── gleam.toml          # Gleam 프로젝트 설정
         ├── manifest.toml       # Gleam 의존성 매니페스트
         ├── build_nif.bat       # Windows NIF 빌드 스크립트
         ├── build_nif.sh        # Linux NIF 빌드 스크립트
         ├── Makefile            # Linux Makefile
         ├── Makefile.win        # Windows Makefile
         └── build               # Gleam 빌드 출력 (생성됨)
            └── dev/erlang/anpr
               ├── ebin          # 컴파일된 .beam 파일
               └── priv          # 런타임용 NIF 라이브러리
  ```

### 2. 사전 요구사항

1. Gleam과 Erlang 설치

   **Windows:**

   ```sh
   # 먼저 Erlang 설치
   # https://www.erlang.org/downloads 에서 다운로드

   # Gleam 설치
   # https://gleam.run/getting-started/installing/ 에서 다운로드
   ```

   **Linux:**

   ```sh
   # Erlang 설치
   sudo apt-get install erlang

   # Gleam 설치 (x86_64)
   cd /tmp
   wget https://github.com/gleam-lang/gleam/releases/download/v1.14.0/gleam-v1.14.0-x86_64-unknown-linux-musl.tar.gz
   tar xzf gleam-v1.14.0-x86_64-unknown-linux-musl.tar.gz
   sudo mv gleam /usr/local/bin/

   # ARM64 (aarch64)용
   wget https://github.com/gleam-lang/gleam/releases/download/v1.14.0/gleam-v1.14.0-aarch64-unknown-linux-musl.tar.gz
   tar xzf gleam-v1.14.0-aarch64-unknown-linux-musl.tar.gz
   sudo mv gleam /usr/local/bin/
   ```

2. 설치 확인

   ```sh
   gleam --version
   erl -version
   ```

### 3. 실행 방법

1. Gleam 예제 디렉토리로 이동

   ```sh
   cd Gleam/anpr
   ```

2. 예제 실행

   ```sh
   # 의존성 설치
   gleam deps download

   # 예제 실행
   gleam run

   # 또는 빌드 후 실행
   gleam build
   gleam run
   ```

### 4. 참고사항

- 이 Gleam 구현은 다른 언어 예제들과 동일한 기능을 제공합니다
- Gleam의 Erlang 상호 운용성을 사용하여 네이티브 TSANPR 라이브러리와 인터페이스합니다
- Gleam의 타입 안전성과 함수형 프로그래밍 패러다임이 신뢰성을 제공합니다
- 뛰어난 동시성을 위해 Erlang 가상 머신(BEAM)에서 실행됩니다
- 참고: 이 예제는 데모를 위한 단순화된 네이티브 라이브러리 통합을 사용합니다

### 5. 기능

- **파일 기반 인식**: 이미지 파일을 직접 처리
- **인코딩된 이미지 처리**: 인코딩된 이미지 데이터 처리 (JPEG, PNG 등)
- **픽셀 버퍼 처리**: 원시 픽셀 데이터 처리 (단순화된 구현)
- **다중 출력 형식**: 텍스트, JSON, YAML, XML, CSV 출력 지원
- **다중 인식 모드**: 단일 번호판, 다중 번호판, 차량 감지 등
- **관심 영역(RoI)**: 이미지 내 특정 영역 처리
- **다국가 지원**: 다양한 번호판 형식 지원 (KR, JP, VN 등)

### 6. API 참조

#### TSANPR 모듈

Gleam 구현은 다음 함수들을 제공합니다:

**초기화:**

- `tsanpr.new(library_path: String) -> Result(TSANPR, String)`: TSANPR 인스턴스 생성

**핵심 함수:**

- `tsanpr.initialize(tsanpr: TSANPR, mode: String) -> Result(String, String)`: ANPR 엔진 초기화
- `tsanpr.read_file(tsanpr: TSANPR, img_file_name: String, output_format: String, options: String) -> Result(String, String)`: 이미지 파일 처리
- `tsanpr.read_pixels(tsanpr: TSANPR, pixels: List(Int), width: Int, height: Int, stride: Int, pixel_format: String, output_format: String, options: String) -> Result(String, String)`: 픽셀 데이터 처리

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
