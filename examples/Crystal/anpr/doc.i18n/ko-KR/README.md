[English](../../README.md) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# Crystal 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Crystal/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Crystal/anpr)

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
  └── Crystal
      └── anpr                   # 소스 디렉토리
         ├── anpr.cr
         └── tsanpr.cr
  ```

### 2. 사전 요구사항

1. Crystal 설치 (버전 1.0.0 이상 권장)

   **Windows:**

   ```sh
   # Scoop 사용
   scoop install crystal

   # 또는 https://crystal-lang.org/install/on_windows/ 에서 다운로드
   ```

   **Linux:**

   ```sh
   # Ubuntu/Debian
   curl -fsSL https://crystal-lang.org/install.sh | sudo bash

   # 또는 패키지 매니저 사용
   sudo apt-get install crystal
   ```

2. 설치 확인

   ```sh
   crystal --version
   ```

### 3. 실행 방법

1. Crystal 예제 디렉토리로 이동

   ```sh
   cd Crystal/anpr
   ```

2. 의존성 설치

   ```sh
   # 필수 shard 설치 (이미지 디코딩을 위한 StumpyPNG 및 StumpyJPEG)
   shards install
   ```

   **Windows 참고사항:** Shards는 심볼릭 링크가 필요합니다. 심볼릭 링크 오류가 발생하면 두 가지 옵션이 있습니다:

   - **옵션 1 (권장):** Windows 설정에서 개발자 모드 활성화
     - 설정 → 개인 정보 및 보안 → 개발자용 → 개발자 모드 → 켜기
     - 참고: https://learn.microsoft.com/ko-kr/windows/apps/get-started/enable-your-device-for-development

   - **옵션 2:** PowerShell 또는 명령 프롬프트를 관리자 권한으로 실행
     ```powershell
     # 관리자 권한으로 실행
     shards install
     ```

3. 예제 실행

   **Shards 사용 (권장):**

   ```sh
   # 빌드 및 실행
   shards build
   ./bin/anpr

   # 최적화와 함께 빌드
   shards build --release
   ```

   **직접 컴파일:**

   ```sh
   # 직접 실행
   crystal run anpr.cr

   # 또는 컴파일 후 실행
   crystal build anpr.cr
   ./anpr

   # 최적화와 함께 컴파일
   crystal build --release anpr.cr
   ```

### 4. 참고사항

- 이 Crystal 구현은 다른 언어 예제들과 동일한 기능을 제공합니다
- Crystal의 `lib` 바인딩을 사용하여 네이티브 TSANPR 라이브러리와 인터페이스합니다
- Crystal의 Ruby와 같은 문법과 C와 같은 성능으로 시스템 프로그래밍에 이상적입니다
- Windows와 Linux에 대한 크로스 플랫폼 지원
- 가비지 컬렉션과 컴파일 타임 null 검사를 통한 메모리 안전성

### 5. 기능

- **파일 기반 인식**: `anpr_read_file`을 사용한 이미지 파일 직접 처리
- **인코딩된 이미지 처리**: 바이트 배열로 인코딩된 이미지 데이터 처리 (JPEG, PNG 등)
- **픽셀 버퍼 처리**: StumpyPNG 및 StumpyJPEG 라이브러리를 사용하여 RGB 형식으로 디코딩된 픽셀 데이터 처리
- **다중 출력 형식**: 텍스트, JSON, YAML, XML, CSV 출력 지원
- **다중 인식 모드**: 단일 번호판, 다중 번호판, 차량 감지 등
- **관심 영역(RoI)**: 이미지 내 특정 영역 처리
- **다국가 지원**: 다양한 번호판 형식 지원 (KR, JP, VN 등)

### 6. 이미지 처리 방법

이 예제는 세 가지 다른 방법으로 이미지를 처리하는 것을 보여줍니다:

1. **read_image_file**: `anpr_read_file`을 사용한 직접 파일 처리 (가장 빠른 방법)
2. **read_encoded_image**: 픽셀 포맷 "encoded"로 인코딩된 이미지 바이트 전달
3. **read_pixel_buffer**: StumpyPNG/StumpyJPEG를 사용하여 원시 RGB 픽셀 데이터로 디코딩한 후 `anpr_read_pixels`에 전달
   - PNG 파일: StumpyPNG를 사용하여 디코딩
   - JPEG 파일: StumpyJPEG를 사용하여 디코딩하며, 디코딩 실패 시 자동으로 인코딩된 형식으로 폴백

방법 전환을 위해 코드에서 `anpr_func` 변수를 수정하세요:

```crystal
# 다음 중 하나를 선택:
anpr_func = ->read_image_file(TSANPR, String, String, String)
# anpr_func = ->read_encoded_image(TSANPR, String, String, String)
# anpr_func = ->read_pixel_buffer(TSANPR, String, String, String)
```

**참고**: `read_pixel_buffer` 함수는 JPEG 디코딩 실패를 자동으로 처리하여 인코딩된 형식으로 폴백함으로써 다양한 JPEG 변형에서 안정적인 작동을 보장합니다.

### 7. API 참조

#### TSANPR 클래스

`TSANPR` 클래스는 다음 메서드들을 제공합니다:

**생성자:**

- `TSANPR.new(library_path : String)`: 네이티브 라이브러리 경로로 초기화

**핵심 메서드:**

- `anpr_initialize(mode : String) : String`: ANPR 엔진 초기화
- `anpr_read_file(img_file_name : String, output_format : String, options : String) : String`: 이미지 파일 처리
- `anpr_read_pixels(pixels : UInt8*, width : UInt64, height : UInt64, stride : Int64, pixel_format : String, output_format : String, options : String) : String`: 픽셀 데이터 처리

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

- Crystal 컴파일러가 올바르게 설치되어 있는지 확인
- 컴파일 중에 라이브러리 경로가 존재하는지 확인
- 최적화된 빌드를 위해 `crystal build --release` 사용

**플랫폼별 문제:**

- **Windows**: Visual C++ 재배포 가능 패키지가 설치되어 있는지 확인
- **Linux**: 필요한 시스템 라이브러리를 설치하고 라이브러리 권한이 올바른지 확인
