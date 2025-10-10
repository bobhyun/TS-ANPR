[English](../../README.md) | 한국어 | [日本語](../ja-JP/README.md) | [Tiếng Việt](../vi-VN/README.md)

# COBOL 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/COBOL/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/COBOL/anpr)

이 예제는 C 래퍼 라이브러리를 사용하여 COBOL 애플리케이션에서 TS-ANPR 엔진을 통합하는 방법을 보여줍니다.

## 아키텍처

COBOL 예제는 계층화된 아키텍처를 사용합니다:

```
COBOL 애플리케이션 (anpr.cbl)
         ↓
C 래퍼 라이브러리 (libtsanpr_cobol.so/.dll)
         ↓
TS-ANPR 엔진 (libtsanpr.so/tsanpr.dll)
```

- **COBOL 애플리케이션**: C 래퍼를 호출하는 메인 프로그램
- **C 래퍼**: TS-ANPR 엔진에 대한 COBOL 친화적 인터페이스 제공
- **TS-ANPR 엔진**: 딥러닝 기반 번호판 인식 엔진

### 1. 엔진 파일 복사

_**[참고]** 이 예제의 경우 다른 예제들과 엔진 파일을 공유하기 위해 `examples/bin/` 디렉토리에 압축 해제하지만 실제 배포시는 일반적으로 응용 프로그램의 실행 파일이 있는 디렉토리에 엔진 파일을 복사합니다._

- 윈도우즈 x86 64비트: `examples/bin/windows-x86_64` 디렉토리에 압축 해제
  ```sh
  tar xvf tsanpr*-windows-x86_64.tar.xz
  ```
- 윈도우즈 x86 32비트: `examples/bin/windows-x86` 디렉토리에 압축 해제
  ```sh
  tar xvf tsanpr*-windows-x86.tar.xz
  ```
- 리눅스 x86 64비트: `examples/bin/linux-x86_64` 디렉토리에 압축 해제
  ```sh
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- 리눅스 arm 64비트: `examples/bin/linux-aarch64` 디렉토리에 압축 해제
  ```sh
  tar xvf tsanpr*-linux-aarch64.tar.xz
  ```

- 디렉토리 구성
  ```
  examples
  ├── bin
  │   ├── windows-x86_64        # Windows (x86_64) 엔진 디렉토리
  │   ├── windows-x86           # Windows (x86) 엔진 디렉토리
  │   ├── linux-x86_64           # Linux (x86_64) 엔진 디렉토리
  │   └── linux-aarch64          # Linux (arm64) 엔진 디렉토리
  ├── img                        # 이미지 디렉토리
  └── COBOL
      └── anpr                   # 소스 디렉토리
         ├── src
         │   ├── c               # C 래퍼 소스 파일
         │   │   ├── tsanpr_cobol.c
         │   │   └── tsanpr_cobol.h
         │   └── cobol           # COBOL 소스 파일
         │       └── anpr.cbl
         ├── bin                 # 빌드 출력 디렉토리
         ├── compile.bat         # Windows 빌드 스크립트
         ├── compile.sh          # Linux 빌드 스크립트
         ├── run.bat             # Windows 실행 스크립트
         ├── run.sh              # Linux 실행 스크립트
         └── Makefile            # GNU Make 빌드 파일
  ```

### 2. 빌드 및 실행

#### 2.1 Windows

1. **GnuCOBOL 설치**
   - [GnuCOBOL for Windows](https://sourceforge.net/projects/gnucobol/) 다운로드 및 설치
   - 또는 [SuperBOL](https://superbol.eu/developers/windows/) 설치 (권장)
   - GnuCOBOL bin 디렉토리를 PATH에 추가

2. **GCC 설치**
   - C 래퍼 빌드에 GCC 필요
   - SuperBOL 설치 시 MinGW64 GCC 포함됨
   - 그 외의 경우 [MinGW-w64](https://www.mingw-w64.org/) 설치

3. **빌드 방법**
   ```cmd
   compile.bat
   ```
   또는 Make 사용:
   ```cmd
   make
   ```

4. **실행 방법**
   ```cmd
   run.bat
   ```
   또는 Make 사용:
   ```cmd
   make run
   ```

#### 2.2 Linux

1. **의존성 설치**
   - Debian / Ubuntu
     ```sh
     sudo apt-get update
     sudo apt-get install gnucobol gcc
     ```
   - Oracle Linux / RHEL / CentOS
     ```sh
     sudo yum install gnucobol gcc
     ```
   - Fedora
     ```sh
     sudo dnf install gnucobol gcc
     ```

2. **빌드 방법**
   ```sh
   chmod +x compile.sh
   ./compile.sh
   ```
   또는 Make 사용:
   ```sh
   make
   ```

3. **실행 방법**
   ```sh
   chmod +x run.sh
   ./run.sh
   ```
   또는 Make 사용:
   ```sh
   make run
   ```

## 구현 세부 사항

### C 래퍼 (`src/c/tsanpr_cobol.c`)

C 래퍼는 세 가지 주요 함수를 제공합니다:
- `tsanpr_cobol_initialize`: ANPR 엔진 초기화
- `tsanpr_cobol_read_file`: 이미지 파일 처리
- `tsanpr_cobol_cleanup`: 리소스 해제

래퍼가 처리하는 기능:
- TS-ANPR 엔진 라이브러리 동적 로딩
- 플랫폼 감지 (Windows/Linux, x86/ARM)
- COBOL 문자열 변환 (고정 길이 → null-terminated)
- 오류 처리 및 결과 포맷팅

### COBOL 프로그램 (`src/cobol/anpr.cbl`)

다음 기능을 시연합니다:
- 단일 번호판 인식
- 다중 번호판 인식 (vm)
- 오토바이 포함 다중 번호판 (vmb)
- 서라운드 감지 (vms)
- 객체 감지 (dms, dmsr)
- 관심 영역(ROI) 감지 (dmsri)

모든 기능이 다른 언어(C, Python 등)의 예제와 동일합니다.

## 성능 참고 사항

- **Windows**: 엔진 초기화 약 1초
- **WSL/Linux on Windows filesystem (`/mnt/`)**: 파일시스템 간 성능 오버헤드로 인해 초기화에 5-7초 소요
- **Native Linux**: 엔진 초기화 약 1-2초

WSL에서 더 나은 성능을 위해 전체 프로젝트를 WSL 네이티브 파일시스템(예: `~/`)으로 복사하세요.

## 문제 해결

### Linux: "libcob: error: module not found"

GnuCOBOL이 C 래퍼 라이브러리를 찾을 수 없을 때 발생합니다. 실행 스크립트는 `LD_PRELOAD`를 사용하여 라이브러리를 강제 로드합니다.

**해결책**: 바이너리를 직접 실행하지 말고 항상 `run.sh` 또는 `make run`을 사용하세요.

### Windows: DLL not found 오류

다음을 확인하세요:
1. 컴파일 후 `bin/tsanpr_cobol.dll`이 존재하는지
2. `COB_LIBRARY_PATH`를 설정하는 `run.bat`을 통해 실행하는지

## 라이선스

이 예제 코드는 MIT 라이선스로 배포됩니다.