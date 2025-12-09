[English](../../README.md) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# Erlang 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Erlang/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Erlang/anpr)

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
  └── Erlang
      └── anpr                   # 프로젝트 디렉토리
         ├── c_src               # C NIF 소스 파일
         │   └── tsanpr_nif.c
         ├── src                 # Erlang 소스 파일
         │   ├── anpr_app.erl    # 애플리케이션 동작
         │   ├── anpr_sup.erl    # 슈퍼바이저 동작
         │   ├── anpr.erl        # 메인 모듈
         │   ├── tsanpr.erl      # NIF 래퍼 모듈
         │   └── anpr.app.src    # 애플리케이션 리소스 파일
         ├── priv                # 컴파일된 NIF 라이브러리
         │   └── tsanpr_nif.dll/.so
         ├── rebar.config        # rebar3 설정
         └── _build              # rebar3 빌드 출력 (생성됨)
            └── default
               └── lib
                  └── anpr
                     ├── ebin    # 컴파일된 .beam 파일
                     └── priv    # NIF 라이브러리
  ```

### 2. 사전 요구사항

1. Erlang/OTP 설치 (버전 24 이상 권장)

   **Windows:**

   ```sh
   # https://www.erlang.org/downloads 에서 다운로드
   # 또는 Chocolatey 사용
   choco install erlang
   ```

   **Linux:**

   ```sh
   # Ubuntu/Debian
   sudo apt-get update
   sudo apt-get install -y erlang

   # Oracle Linux / RHEL / CentOS (8/9+)
   sudo dnf install -y erlang || sudo yum install -y erlang
   # 팁: 패키지가 없으면 EPEL 활성화 또는 Erlang Solutions 저장소 사용:
   # https://www.erlang.org/downloads

   # 또는 kerl 사용 (권장)
   curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
   chmod a+x kerl
   ./kerl build 24.3 24.3
   ./kerl install 24.3 ~/erlang/24.3
   # 현재 셸에 활성화
   . ~/erlang/24.3/activate
   # 비활성화: deactivate
   ```

2. rebar3 설치

   **Windows:**

   GitHub에서 최신 rebar3 escript 다운로드:
   ```powershell
   # 최신 rebar3 다운로드 (Erlang/OTP 28+ 호환)
   Invoke-WebRequest -Uri "https://github.com/erlang/rebar3/releases/latest/download/rebar3" -OutFile "rebar3"
   
   # rebar3를 프로젝트 디렉토리에 배치하거나 PATH의 디렉토리로 이동
   ```

   **참고:** Windows에서는 `escript rebar3 <명령어>` 형식으로 실행해야 합니다 (`rebar3 <명령어>`가 아님).

   **Linux:**

   ```sh
   # Ubuntu/Debian
   sudo apt-get install rebar3
   
   # 또는 GitHub에서 최신 버전 다운로드
   wget https://github.com/erlang/rebar3/releases/latest/download/rebar3
   chmod +x rebar3
   sudo mv rebar3 /usr/local/bin/
   ```

3. C 컴파일러 설치 (NIF 빌드에 필요)

   **Windows:**
   - C++ 개발 도구가 포함된 Visual Studio 설치, 또는
   - MinGW-w64 설치: `choco install mingw`

   **Linux:**
   ```sh
   # Ubuntu/Debian
   sudo apt-get install build-essential

   # Fedora/CentOS
   sudo yum groupinstall "Development Tools"
   ```

4. 설치 확인

   ```sh
   erl -version
   rebar3 version
   gcc --version  # Windows에서 Visual Studio 사용 시 cl.exe
   ```

### 3. 빌드 및 실행

1. Erlang 예제 디렉토리로 이동

   ```sh
   cd examples/Erlang/anpr
   ```

2. NIF (Native Implemented Function) 라이브러리 빌드

   **Windows:**

   "x64 Native Tools Command Prompt for VS"를 열거나 vcvars64.bat를 실행한 후:
   ```cmd
   build_nif.bat
   ```

   다음을 수행합니다:
   - Erlang 설치 자동 감지
   - MSVC를 사용하여 `c_src/tsanpr_nif.c` 컴파일
   - `priv/tsanpr_nif.dll` 생성

   **Linux:**

   ```sh
   make priv/tsanpr_nif.so
   ```

   다음을 수행합니다:
   - gcc를 사용하여 `c_src/tsanpr_nif.c` 컴파일
   - libdl을 적절히 링크하여 `priv/tsanpr_nif.so` 생성

3. Erlang 애플리케이션 빌드

   **Windows (escript 명령어 사용):**
   ```cmd
   escript rebar3 compile
   ```

   **Linux:**
   ```sh
   rebar3 compile
   ```

   다음을 수행합니다:
   - Erlang 모듈(`anpr.erl`, `tsanpr.erl`)을 `_build/default/lib/anpr/ebin/`로 컴파일
   - `priv/`의 NIF 라이브러리를 `_build/default/lib/anpr/priv/`로 복사

4. 애플리케이션 실행

   ```sh
   # 비대화형 모드
   erl -pa _build/default/lib/anpr/ebin -noshell -eval "anpr:main()" -s init stop

   # 대화형 모드
   erl -pa _build/default/lib/anpr/ebin
   1> anpr:main().
   ```

### 4. 참고사항

- 이 Erlang 구현은 다른 언어 예제들과 동일한 기능을 제공합니다
- Erlang의 NIF(Native Implemented Functions)를 사용하여 네이티브 TSANPR 라이브러리와 인터페이스합니다
- Erlang의 내결함성과 동시성 모델로 고가용성 시스템에 이상적입니다
- Windows와 Linux에 대한 크로스 플랫폼 지원
- 참고: 이것은 데모 목적을 위한 단순화된 NIF 구현입니다

### 5. 기능

- **파일 기반 인식**: 이미지 파일을 직접 처리
- **인코딩된 이미지 처리**: 인코딩된 이미지 데이터 처리 (JPEG, PNG 등)
- **픽셀 버퍼 처리**: 원시 픽셀 데이터 처리 (단순화된 구현)
- **다중 출력 형식**: 텍스트, JSON, YAML, XML, CSV 출력 지원
- **다중 인식 모드**: 단일 번호판, 다중 번호판, 차량 감지 등
- **관심 영역(RoI)**: 이미지 내 특정 영역 처리
- **다국가 지원**: 다양한 번호판 형식 지원 (KR, JP, VN 등)

### 6. API 참조

#### tsanpr 모듈

`tsanpr` 모듈은 다음 함수들을 제공합니다:

**초기화:**

- `tsanpr:new(LibraryPath)`: 네이티브 라이브러리 경로로 초기화

**핵심 함수:**

- `tsanpr:anpr_initialize(Tsanpr, Mode)`: ANPR 엔진 초기화
- `tsanpr:anpr_read_file(Tsanpr, ImgFileName, OutputFormat, Options)`: 이미지 파일 처리
- `tsanpr:anpr_read_pixels(Tsanpr, Pixels, Width, Height, Stride, PixelFormat, OutputFormat, Options)`: 픽셀 데이터 처리

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
