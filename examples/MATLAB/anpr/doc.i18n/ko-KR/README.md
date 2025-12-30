[English](../../README.md) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# MATLAB/Octave 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/MATLAB/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/MATLAB/anpr)

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
  └── MATLAB
      └── anpr                   # 프로젝트 디렉토리
         ├── src                 # 소스 디렉토리
         │   ├── anpr.m          # 메인 예제 스크립트
         │   ├── TSANPR.m        # TSANPR 래퍼 클래스
         │   └── mex             # MEX 소스 디렉토리
         │       ├── tsanpr_mex.c    # MEX 소스 파일
         │       └── build_mex.m     # MEX 빌드 스크립트
         └── doc.i18n            # 번역된 문서
  ```

### 2. 사전 요구사항

#### 옵션 A: MATLAB (상용)

1. MATLAB 설치 (R2018b 이상 권장)

   **Windows:**
   - https://www.mathworks.com/downloads/ 에서 MATLAB 다운로드 및 설치
   - 유효한 라이선스 필요

   **Linux:**
   - https://www.mathworks.com/downloads/ 에서 MATLAB 다운로드 및 설치
   - 유효한 라이선스 필요

2. C 컴파일러 설정
   ```matlab
   mex -setup
   ```

3. 설치 확인
   ```matlab
   version
   mex -setup
   ```

#### 옵션 B: GNU Octave (무료, 오픈소스)

GNU Octave는 MATLAB과 대부분 호환되는 무료 대안입니다.

**Windows:**

```cmd
# winget 사용
winget install GNU.Octave

# 또는 https://octave.org/download 에서 다운로드
```

**Linux (Ubuntu/Debian):**

```sh
sudo apt-get update
sudo apt-get install -y octave octave-image liboctave-dev
```

**Linux (Fedora/RHEL):**

```sh
sudo dnf install -y octave octave-image octave-devel
```

설치 확인:
```sh
octave --version
```

### 3. MEX 빌드 방법

MEX 파일은 첫 실행 시 자동으로 빌드됩니다. 수동으로 빌드하려면:

#### MATLAB 사용

```matlab
cd examples/MATLAB/anpr/src/mex
build_mex
```

#### GNU Octave 사용

```sh
cd examples/MATLAB/anpr/src/mex
octave --eval "build_mex"
```

### 4. 실행 방법

#### MATLAB 사용

1. 소스 디렉토리로 이동
   ```sh
   cd examples/MATLAB/anpr/src
   ```

2. MATLAB을 시작하고 예제 실행
   ```matlab
   % 메인 ANPR 예제 실행
   anpr
   ```

#### GNU Octave 사용

**Windows:**

```cmd
cd examples\MATLAB\anpr\src
octave --eval "anpr"
```

**Linux:**

```sh
cd examples/MATLAB/anpr/src
octave --eval "anpr"
```

또는 대화형으로 실행:
```sh
octave
```
```octave
cd examples/MATLAB/anpr/src
anpr
```

### 5. 대화형 사용

```matlab
% 경로 추가
addpath('mex');

% TSANPR 초기화
engine_path = '../../../bin/windows-x86_64/tsanpr.dll';  % Windows
% engine_path = '../../../bin/linux-x86_64/libtsanpr.so';  % Linux

tsanpr = TSANPR(engine_path);

% 엔진 초기화
error_msg = tsanpr.anpr_initialize('text;country=KR');
if ~isempty(error_msg)
    fprintf('오류: %s\n', error_msg);
end

% 이미지 처리
result = tsanpr.anpr_read_file('../../../img/KR/licensePlate.jpg', 'json', '');
fprintf('결과: %s\n', result);
```

### 6. 참고사항

- 이 구현은 네이티브 라이브러리 통합을 위해 MEX 파일을 사용합니다
- MEX 파일은 MATLAB과 Octave 모두에서 일관된 인터페이스를 제공합니다
- MEX 파일이 없으면 첫 실행 시 자동으로 빌드됩니다
- Windows 및 Linux 크로스 플랫폼 지원

### 7. 기능

- **파일 기반 인식**: 이미지 파일을 직접 처리
- **인코딩된 이미지 처리**: 인코딩된 이미지 데이터 처리 (JPEG, PNG 등)
- **픽셀 버퍼 처리**: 이미지 처리 함수를 사용한 원시 픽셀 데이터 처리
- **다중 출력 형식**: 텍스트, JSON, YAML, XML, CSV 출력 지원
- **다중 인식 모드**: 단일 번호판, 다중 번호판, 차량 감지 등
- **관심 영역(RoI)**: 이미지 내 특정 영역 처리
- **다국가 지원**: 다양한 번호판 형식 지원 (KR, JP, VN 등)

### 8. API 참조

#### TSANPR 클래스

**생성자:**
- `TSANPR(library_path)`: 네이티브 라이브러리 경로로 초기화

**핵심 메서드:**
- `anpr_initialize(mode)`: ANPR 엔진 초기화
- `anpr_read_file(img_file_name, output_format, options)`: 이미지 파일 처리
- `anpr_read_pixels(pixels, width, height, stride, pixel_format, output_format, options)`: 픽셀 데이터 처리

**정적 메서드:**
- `TSANPR.isOctave()`: GNU Octave에서 실행 중인지 확인

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

### 9. 문제 해결

**MEX 빌드 문제:**
- C 컴파일러가 설치되어 있는지 확인
  - **MATLAB**: `mex -setup` 실행하여 설정
  - **Octave Windows**: MinGW가 Octave 설치에 포함되어 있음
  - **Octave Linux**: `liboctave-dev` 또는 `octave-devel` 설치
- 컴파일러 사용 가능 확인: `mex -setup` (MATLAB) 또는 `mkoctfile --version` (Octave)

**라이브러리 로딩 문제:**
- TSANPR 라이브러리 경로가 올바른지 확인
- 모든 시스템 의존성이 설치되어 있는지 확인
- Linux에서 `LD_LIBRARY_PATH`에 엔진 디렉토리 포함 확인

**플랫폼별 문제:**
- **Windows**: Visual C++ 재배포 가능 패키지가 설치되어 있는지 확인
- **Linux**: 필요한 시스템 라이브러리를 설치하고 라이브러리 권한이 올바른지 확인
