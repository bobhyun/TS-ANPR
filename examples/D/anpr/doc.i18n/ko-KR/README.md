[English](../../README.md) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# D 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/D/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/D/anpr)

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
  └── D
      └── anpr                   # 소스 디렉토리
         ├── anpr.d
         ├── dub.json
         └── tsanpr.d
  ```

### 2. 사전 요구사항

1. D 컴파일러 설치 (DMD, LDC, 또는 GDC)

   **Windows:**

   ```sh
   # https://dlang.org/download.html 에서 DMD 다운로드 및 설치
   # 또는 chocolatey 사용
   choco install dmd
   ```

   **Ubuntu / Debian:**

   ```sh
   # snap을 사용하여 설치
   sudo snap install dmd --classic
   sudo snap install dub --classic

   # 또는 apt를 사용하여 설치
   sudo wget https://netcologne.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list
   sudo apt-get update && sudo apt-get -y --allow-unauthenticated install --reinstall d-apt-keyring
   sudo apt-get update && sudo apt-get install dmd-compiler dub
   ```

   **Oracle Linux / RedHat (RHEL) / CentOS:**

   ```sh
   # snap을 사용하여 설치
   sudo snap install dmd --classic
   sudo snap install dub --classic

   # 또는 https://dlang.org/download.html 에서 직접 다운로드 및 설치
   curl -fsS https://dlang.org/install.sh | bash -s dmd
   source ~/dlang/dmd-*/activate
   ```

2. 설치 확인

   ```sh
   dmd --version
   ```

### 3. 실행 방법

1. D 예제 디렉토리로 이동

   ```sh
   cd D/anpr
   ```

2. 예제 컴파일 및 실행

   **DUB 사용 (권장):**

   ```sh
   # 빌드 및 실행
   dub run

   # 빌드만 수행
   dub build

   # 릴리스 버전 빌드
   dub build --build=release

   # 실행
   ./bin/anpr
   ```

### 4. 참고사항

- 이 D 구현은 다른 언어 예제들과 동일한 기능을 제공합니다
- D의 뛰어난 C 상호 운용성을 사용하여 네이티브 TSANPR 라이브러리와 인터페이스합니다
- D의 시스템 프로그래밍 기능과 현대적인 언어 특성이 성능과 안전성을 모두 제공합니다
- Windows와 Linux에 대한 크로스 플랫폼 지원
- D의 내장 메모리 관리와 가비지 컬렉션이 개발을 단순화합니다

### 5. 기능

- **파일 기반 인식**: 이미지 파일을 직접 처리
- **인코딩된 이미지 처리**: 인코딩된 이미지 데이터 처리 (JPEG, PNG 등)
- **픽셀 버퍼 처리**: 원시 픽셀 데이터 처리 (imageformats 라이브러리 사용)
- **다중 출력 형식**: 텍스트, JSON, YAML, XML, CSV 출력 지원
- **다중 인식 모드**: 단일 번호판, 다중 번호판, 차량 감지 등
- **관심 영역(RoI)**: 이미지 내 특정 영역 처리
- **다국가 지원**: 다양한 번호판 형식 지원 (KR, JP, VN 등)

### 6. API 참조

#### TSAnpr 클래스

**생성자:**

- `this(string libraryPath)`: 네이티브 라이브러리 경로로 초기화

**핵심 메서드:**

- `string anprInitialize(string enginePath)`: ANPR 엔진 초기화
- `string anprReadFile(string imagePath, string options, string outputFormat)`: 이미지 파일 처리
- `string anprReadPixels(ubyte[] pixels, int width, int height, int channels, string options, string outputFormat)`: 픽셀 데이터 처리

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

**컴파일 문제:**

- D 컴파일러가 올바르게 설치되어 있는지 확인
- 대상 플랫폼에 적합한 컴파일러 플래그 사용
- 복잡한 프로젝트의 경우 DUB 패키지 매니저 사용 고려

**플랫폼별 문제:**

- **Windows**: Visual C++ 재배포 가능 패키지가 설치되어 있는지 확인
- **Linux**: 필요한 시스템 라이브러리를 설치하고 라이브러리 권한이 올바른지 확인
