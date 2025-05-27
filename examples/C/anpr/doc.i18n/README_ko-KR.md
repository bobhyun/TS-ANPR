[English](../README.md) | [日本語](README_ja-JP.md) | 한국어 | [Tiếng Việt](README_vi-VN.md)

# C 예제

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
  │   ├─── windows-x86_64       # engine directory for Windows (x86_64)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86          # engine directory for Windows (x86)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64          # engine directory for Linux (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64         # engine directory for Linux (arm64)
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                       # image directory
  └── C
      └── anpr                  # source directory
          ├── CMakeLists.txt
          ├── toolchain-aarch64.cmake
          ├── src
          │   ├── tsanpr.h
          │   ├── tsanpr.c
          │   └── anpr.c
          ├── bin               # compiled executable
          │   ├── windows-x86-64
          │   │   ├── Debug
          │   │   │   └── anpr.exe
          │   │   └── Release
          │   │       └── anpr.exe
          │   ├── windows-x86
          │   │   ├── Debug
          │   │   │   └── anpr.exe
          │   │   └── Release
          │   │       └── anpr.exe
          │   ├── mingw-x86-64
          │   │   └── anpr.exe
          │   ├── mingw-x86
          │   │   └── anpr.exe
          │   ├── linux-x86-64
          │   │   └── anpr
          │   └── linux-aarch64
          │       └── anpr
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

   # libpng libjpeg 설치
   vcpkg install libpng:x64-windows libjpeg-turbo:x64-windows
   cd ..

   cmake ../.. -G "Visual Studio 17 2022" -A x64 -DCMAKE_TOOLCHAIN_FILE=vcpkg/scripts/buildsystems/vcpkg.cmake
   cmake --build . --config Debug   # Debug build
   cmake --build . --config Release # Release build
   cd ..\..

   # 윈도우즈에서 non-ASCII 문자가 깨지는 경우 실행
   chcp 65001

   # Run
   bin\windows-x86_64\Debug\anpr.exe
   bin\windows-x86_64\Release\anpr.exe
   ```

2. Windows 32bit (MSVC, x86 Native Tools Command Prompt)

   ```sh
   mkdir build\windows-x86
   cd build\windows-x86

   # vcpkg 설치
   git clone https://github.com/microsoft/vcpkg.git
   cd vcpkg
   bootstrap-vcpkg.bat

   # libpng libjpeg 설치
   vcpkg install libpng:x86-windows libjpeg-turbo:x86-windows
   cd ..

   cmake ../.. -G "Visual Studio 17 2022" -A Win32  -DCMAKE_TOOLCHAIN_FILE=vcpkg/scripts/buildsystems/vcpkg.cmake
   cmake --build . --config Debug   # Debug build
   cmake --build . --config Release # Release build
   cd ..\..

   # 윈도우즈에서 non-ASCII 문자가 깨지는 경우 실행
   chcp 65001

   # Run
   bin\windows-x86\Debug\anpr.exe
   bin\windows-x86\Release\anpr.exe
   ```

3. Windows MinGW64 (MSYS2)

   ```sh
   # 종속성 설치
   pacman -Syu
   pacman -S mingw-w64-x86_64-gcc mingw-w64-x86_64-libpng mingw-w64-x86_64-libjpeg-turbo

   mkdir build/windows-x86_64
   cd build/windows-x86_64

   # build
   cmake ../.. -G "MinGW Makefiles"
   mingw32-make
   cd ../..

   # Run
   bin/mingw-x86_64/anpr
   ```

4. Windows MinGW32 (MSYS2)

   ```sh
   # 종속성 설치
   pacman -Syu
   pacman -S mingw-w64-i686-gcc mingw-w64-i686-libpng mingw-w64-i686-libjpeg-turbo

   mkdir build/windows-x86
   cd build/windows-x86

   # build
   cmake ../.. -G "MinGW Makefiles"
   mingw32-make
   cd ../..

   # Run
   bin/mingw-x86/anpr
   ```

5. Linux x86_64

   ```sh
   # 종속성 설치
   # Debian / Ubuntu Linux
   sudo apt update
   sudo apt install build-essential libpng-dev libjpeg-dev

   # Oracle Linux / RedHat (RHEL) / CentOS
   sudo yum install epel-release
   sudo yum install gcc make libpng-devel libjpeg-devel

   mkdir -p build/linux-x86_64
   cd build/linux-x86_64

   # Debug build
   cmake ../.. -DCMAKE_BUILD_TYPE=Debug
   make
   cd ../..

   # Release build
   cmake ../.. -DCMAKE_BUILD_TYPE=Release
   make
   cd ../..

   # Run
   bin/linux-x86_64/anpr
   ```

6. Linux aarch64 (ARM64)

   ```sh
   # 종속성 설치
   # Debian / Ubuntu Linux
   sudo apt update
   sudo apt install build-essential libpng-dev libjpeg-dev

   # Oracle Linux / RedHat (RHEL) / CentOS
   sudo yum install epel-release
   sudo yum install gcc make libpng-devel libjpeg-devel

   mkdir -p build/linux-aarch64
   cd build/linux-aarch64

   # Debug build
   cmake ../.. -DCMAKE_BUILD_TYPE=Debug
   make
   cd ../..

   # Release build
   cmake ../.. -DCMAKE_BUILD_TYPE=Release
   make
   cd ../..

   # Run
   bin/linux-aarch64/anpr
   ```
