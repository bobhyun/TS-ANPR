[English](../../README.md) | 한국어 | [日本語](../ja-JP/README.md) | [Tiếng Việt](../vi-VN/README.md)

# Pascal 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Pascal/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Pascal/anpr)

### 1. 엔진 파일 복사

_**[참고]** 이 예제의 경우 다른 예제들과 엔진 파일을 공유하기 위해 `examples/bin/` 디렉토리에 압축 해제하지만 실제 배포시는 일반적으로 응용 프로그램의 실행 파일이 있는 디렉토리에 엔진 파일을 복사합니다._

- 윈도우즈 x86 64비트
  엔진 파일을 `examples/bin/windows-x86_64` 디렉토리에 압축 해제
  ```sh
  7z x tsanpr*-windows-x86_64.7z
  ```
- 윈도우즈 x86 32비트
  엔진 파일을 `examples/bin/windows-x86` 디렉토리에 압축 해제
  ```sh
  7z x tsanpr*-windows-x86.7z
  ```
- 리눅스 x86 64비트
  엔진 파일을 `examples/bin/linux-x86_64` 디렉토리에 압축 해제
  ```sh
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- 리눅스 arm 64비트
  엔진 파일을 `examples/bin/linux-aarch64` 디렉토리에 압축 해제
  ```sh
  tar xvf tsanpr*-linux-aarch64.tar.xz
  ```
- 디렉토리 구성
  ```sh
  examples
  ├── bin
  │   ├─── windows-x86_64        # Windows (x86_64) 엔진 디렉토리
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86           # Windows (x86) 엔진 디렉토리
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64           # Linux (x86_64) 엔진 디렉토리
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # Linux (arm64) 엔진 디렉토리
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # 이미지 디렉토리
  └── Pascal
      └── anpr                   # 프로젝트 디렉토리
         ├── src                 # 메인 소스 디렉토리
         │   └── anpr.pas        # 메인 프로그램
         ├── units               # 유닛 디렉토리
         │   ├── tsanpr.pas      # TSANPR 유닛
         │   └── stb_image.pas   # stb_image 바인딩
         ├── lib                 # 외부 라이브러리 소스
         │   ├── stb_image.h     # stb_image 헤더
         │   └── stb_image_lib.c # stb_image 공유 라이브러리 소스
         ├── bin                 # 출력 디렉토리 (빌드 생성)
         ├── compile.bat
         ├── compile.sh
         └── Makefile
  ```

### 2. 빌드 및 실행

#### 2.1 Windows

1. Free Pascal 설치

   - [Free Pascal](https://www.freepascal.org/download.html) 다운로드 및 설치
   - PATH에 Free Pascal bin 디렉토리 추가

2. 빌드 방법

   ```cmd
   compile.bat
   ```

3. 실행 방법

   ```cmd
   cd bin
   anpr.exe
   ```

#### 2.2 Linux

1. 종속성 설치

   - Debian / Ubuntu Linux

     ```sh
     sudo apt-get update
     sudo apt-get install fpc
     ```

   - Oracle Linux / RedHat (RHEL) / CentOS

     ```sh
     sudo yum install fpc
     ```

   - Fedora

     ```sh
     sudo dnf install fpc
     ```

2. 빌드 방법

   ```sh
   chmod +x compile.sh
   ./compile.sh
   ```

   또는 Make 사용:

   ```sh
   make
   ```

3. 실행 방법

   ```sh
   cd bin
   ./anpr
   ```
