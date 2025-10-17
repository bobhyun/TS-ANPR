[English](../../) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# Swift 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Swift/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Swift/anpr)

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
  └── Swift
      └── anpr                  # source directory
          ├── Package.swift
          ├── CBridge
          │   ├── tsanpr.h
          │   ├── tsanpr.cpp
          │   ├── CBridge.h
          │   ├── CBridge.cpp
          │   ├── module.modulemap
          │   ├── CMakeLists.txt
          │   └── toolchain-aarch64.cmake
          ├── Sources
          │   ├── Main.swift
          │   └── TSANPR.swift
          ├── .build             # compiled executable
          │   ├── x86_64-unknown-windows-msvc
          │   │   ├── debug
          │   │   │   ├── anpr.exe
          │   │   │   └── CBridge.dll
          │   │   └── release
          │   │       ├── anpr.exe
          │   │       └── CBridge.dll
          │   └── x86_64-unknown-linux-gnu
          │       ├── debug
          │       │   ├── anpr
          │       │   └── libCBridge.so
          │       └── release
          │           ├── anpr
          │           └── libCBridge.so
          └── build             # temporary build directory
  ```

### 2. 빌드 및 실행 방법

1. Windows 64bit (MSVC, x64 Native Tools Command Prompt)

   ```sh
   mkdir build\windows-x86_64
   cd build\windows-x86_64

   # vcpkg 설치
   git clone https://github.com/microsoft/vcpkg.git
   cd vcpkg
   bootstrap-vcpkg.bat

   # opencv 설치
   vcpkg install opencv:x64-windows
   cd ..

   cmake ../../CBridge -G "Visual Studio 17 2022" -A x64 -DCMAKE_TOOLCHAIN_FILE=vcpkg/scripts/buildsystems/vcpkg.cmake
   cmake --build . --config Debug   # Debug build
   cmake --build . --config Release # Release build
   cd ..\..
   swift build             # Debug build
   swift build -c release  # Release build

   # 윈도우즈에서 non-ASCII 문자가 깨지는 경우 실행
   chcp 65001

   # Run
   .build\x86_64-unknown-windows-msvc\debug\anpr.exe
   .build\x86_64-unknown-windows-msvc\release\anpr.exe
   ```

2. Linux x86_64

   ```sh
   # 종속성 설치
   # Debian / Ubuntu Linux
   sudo apt update
   sudo apt install libopencv-dev

   # Oracle Linux / RedHat (RHEL) / CentOS
   sudo yum install epel-release
   sudo yum install opencv-devel

   mkdir -p build/linux-x86_64
   cd build/linux-x86_64

   # Debug build
   cmake ../../CBridge -DCMAKE_BUILD_TYPE=Debug
   make

   # Release build
   cmake ../../CBridge -DCMAKE_BUILD_TYPE=Release
   make
   cd ../..

   swift build             # Debug build
   swift build -c release  # Release build

   # Run
   .build/x86_64-unknown-linux-gnu/debug/anpr
   .build/x86_64-unknown-linux-gnu/release/anpr
   ```

3. Linux aarch64 (ARM64)

   ```sh
   # 종속성 설치
   # Debian / Ubuntu Linux
   sudo apt update
   sudo apt install libopencv-dev

   # Oracle Linux / RedHat (RHEL) / CentOS
   sudo yum install epel-release
   sudo yum install opencv-devel

   mkdir -p build/linux-aarch64
   cd build/linux-aarch64

      # Debug build
   cmake ../../CBridge -DCMAKE_BUILD_TYPE=Debug
   make

   # Release build
   cmake ../../CBridge -DCMAKE_BUILD_TYPE=Release
   make
   cd ../..

   swift build             # Debug build
   swift build -c release  # Release build

   # Run
   .build/aarch64-unknown-linux-gnu/debug/anpr
   .build/aarch64-unknown-linux-gnu/release/anpr
   ```
