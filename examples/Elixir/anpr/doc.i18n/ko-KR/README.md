[English](../../README.md) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# Elixir 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Elixir/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Elixir/anpr)

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
  └── Elixir
      └── anpr                   # 프로젝트 디렉토리
         ├── c_src               # C NIF 소스 파일
         │   └── tsanpr_nif.c
         ├── lib                 # Elixir 소스 파일
         │   ├── anpr.ex         # 메인 모듈
         │   └── tsanpr.ex       # NIF 래퍼 모듈
         ├── priv                # NIF 라이브러리 (빌드 산출물)
         │   └── tsanpr_nif.dll/.so
         ├── mix.exs             # Mix 프로젝트 설정
         ├── Makefile            # 빌드 설정 (Linux)
         ├── Makefile.win        # 빌드 설정 (Windows, nmake)
         ├── build_nif.bat       # 빌드 스크립트 (Windows)
         └── _build              # Mix 빌드 출력 (생성됨)
  ```

### 2. 사전 요구사항

1. Elixir 설치 (버전 1.12 이상 권장)

   **Windows:**

   ```sh
   # Chocolatey 사용
   choco install elixir

   # 또는 https://elixir-lang.org/install.html#windows 에서 다운로드
   ```

   **Linux:**

   ```sh
   # Ubuntu/Debian
   sudo apt-get install elixir

   # 또는 asdf 사용 (권장)
   asdf plugin add elixir
   asdf install elixir latest
   ```

2. 설치 확인

   ```sh
   elixir --version
   ```

### 3. 빌드 및 실행

1. Elixir 예제 디렉토리로 이동

   ```sh
   cd Elixir/anpr
   ```

2. NIF 빌드 (네이티브 라이브러리)

   **Windows:**

   "x64 Native Tools Command Prompt for VS 2022"를 열어 `cl`/`link`가 PATH에 존재하도록 한 뒤:

   ```cmd
   build_nif.bat
   ```

   위 스크립트는 다음을 수행합니다:
   - Erlang 설치 경로 자동 탐지
   - MSVC로 `c_src/tsanpr_nif.c` 컴파일
   - `priv/tsanpr_nif.dll` 생성

   참고: `mix compile`이 `elixir_make`를 통해 Windows에서 `nmake`를 호출하는 경우, `Makefile.win`이 자동으로 실행되며 내부에서 `build_nif.bat`를 호출해 Windows의 인용(quoting) 문제를 피합니다.

   **Linux:**

   ```sh
   make priv/tsanpr_nif.so
   ```

   - gcc로 `c_src/tsanpr_nif.c` 컴파일
   - `priv/tsanpr_nif.so` 생성

3. 의존성 설치 및 Elixir 애플리케이션 컴파일

   ```sh
   mix deps.get
   mix compile
   ```

4. 예제 실행

   ```sh
   # Mix로 실행
   mix run -e "ANPR.main()"

   # 또는 iex (대화형)
   iex -S mix
   iex> ANPR.main()
   ```

### 4. 참고사항

- 이 Elixir 구현은 다른 언어 예제들과 동일한 기능을 제공합니다
- Elixir의 NIF(Native Implemented Functions)를 사용하여 네이티브 TSANPR 라이브러리와 인터페이스합니다
- Elixir의 내결함성 설계와 액터 모델로 분산 시스템에 뛰어납니다
- Windows와 Linux에 대한 크로스 플랫폼 지원
- 참고: 이것은 데모 목적을 위한 단순화된 NIF 구현입니다

추가 구현 노트:
- Windows에서는 `ANPR.get_engine_file_name/0`이 64비트 BEAM(`:erlang.system_info(:wordsize) == 8`)일 경우 `examples/bin/windows-x86_64/tsanpr.dll`을 선택합니다. 그렇지 않으면 `windows-x86`을 사용합니다.
- NIF 모듈은 `c_src/tsanpr_nif.c`에서 `Elixir.TSANPR`로 등록되어 있으며, `lib/tsanpr.ex`에서 베이스 이름(`tsanpr_nif`)으로 로드하여 플랫폼 확장자를 자동으로 선택합니다.

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

`TSANPR` 모듈은 다음 함수들을 제공합니다:

**초기화:**

- `TSANPR.new(library_path)`: 네이티브 라이브러리 경로로 초기화

**핵심 함수:**

- `TSANPR.anpr_initialize(tsanpr, mode)`: ANPR 엔진 초기화
- `TSANPR.anpr_read_file(tsanpr, img_file_name, output_format, options)`: 이미지 파일 처리
- `TSANPR.anpr_read_pixels(tsanpr, pixels, width, height, stride, pixel_format, output_format, options)`: 픽셀 데이터 처리

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

### 7. 문제 해결

**라이브러리 로딩 문제:**

- TSANPR 라이브러리 경로가 올바른지 확인
- 모든 시스템 종속성이 설치되어 있는지 확인
- 라이브러리 권한이 올바른지 확인

**NIF 문제:**

- 이 예제는 데모를 위한 단순화된 NIF 접근 방식을 사용합니다
- 프로덕션 사용을 위해서는 적절한 C NIF를 구현해야 합니다
- Erlang 개발 헤더가 설치되어 있는지 확인

**플랫폼별 문제:**

- **Windows**: Visual C++ 재배포 가능 패키지가 설치되어 있는지 확인
- **Linux**: 필요한 시스템 라이브러리를 설치하고 라이브러리 권한이 올바른지 확인
