[English](../README.md) | 한국어 | [日本語](README_ja-JP.md) | [Tiếng Việt](README_vi-VN.md)

# F#(.NET) 예제

### 1. 엔진 파일 복사

_**[참고]** 이 예제의 경우 다른 예제들과 엔진 파일을 공유하기 위해 `examples/bin/` 디렉토리에 압축 해제하지만 실제 배포시는 일반적으로 응용 프로그램의 실행 파일이 있는 디렉토리에 엔진 파일을 복사합니다._

- 윈도우즈 x86 64비트
  엔진 파일을 `examples/bin/windows-x86_64` 디렉토리에 압축 해제
  ```sh
  unzip tsanpr*-windows-x86_64.zip
  ```
- 윈도우즈 x86 32비트
  엔진 파일을 `examples/bin/windows-x86` 디렉토리에 압축 해제
  ```sh
  unzip tsanpr*-windows-x86.zip
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
  │   ├─── windows-x86_64        # engine directory for Windows (x86_64)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86           # engine directory for Windows (x86)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64           # engine directory for Linux (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # engine directory for Linux (arm64)
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                       # image directory
  └── F#
      └── anpr                  # source directory
          ├── anpr.sln
          └── anpr
              ├── anpr.fsproj
              ├── Program.fs
              ├── TSANPR.fs
              └── bin           # compiled executable directory
                 ├── Release
                 │    └── net8.0
                 │        ├── anpr.exe
                 │        ├── anpr.dll
                 │        ├── FSharp.Core.dll
                 │        └──OpenCvSharp.dll
                 └── Debug
                      └── net8.0
                          ├── anpr.exe
                          ├── anpr.dll
                          ├── FSharp.Core.dll
                          └──OpenCvSharp.dll
  ```

### 2. 종속성 설치

- Windows (NuGet)
  ```sh
  dotnet add package OpenCvSharp4
  dotnet add package OpenCvSharp4.runtime.win
  ```
- x86 Linux
  ```sh
  dotnet add package OpenCvSharp4
  dotnet add package OpenCvSharp4.runtime.linux-x64
  ```
- ARM Linux
  ```sh
  dotnet add package OpenCvSharp4
  dotnet add package OpenCvSharp4.runtime.linux-arm
  ```
