English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# C Example

https://github.com/bobhyun/TS-ANPR/tree/main/examples/C/anpr

### 1. Copying the Engine Files

_**[Note]** In this example, the engine file is extracted to the examples/bin/ directory to share it with other examples. However, for actual deployment, the engine file is typically copied to the directory where the application's executable file is located._

- For Windows x86 64-bit
  Extract the engine file to the `examples/bin/linux-x86_64` directory
  ```sh
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- For Windows x86 32-bit
  Extract the engine file to the `examples/bin/linux-x86` directory
  ```sh
  tar xvf tsanpr*-linux-x86.tar.xz
  ```
- For Linux x86 64-bit
  Extract the engine file to the `examples/bin/linux-x86_64` directory
  ```sh
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- For Linux arm 64-bit
  Extract the engine file to the `examples/bin/linux-aarch64` directory
  ```sh
  tar xvf tsanpr*-linux-aarch64.tar.xz
  ```
- Directory structure
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
  └── C++
      └── anpr                  # source directory
          ├── CMakeLists.txt
          ├── toolchain-aarch64.cmake
          ├── src
          |   ├── tsanpr.h
          |   ├── tsanpr.c
          |   └── anpr.c
          ├── bin               # compiled executable
          |   ├── windows-x86-64
          |   |   ├── Debug
          |   |   |   └── anpr.exe
          |   |   └── Release
          |   |       └── anpr.exe
          |   ├── windows-x86
          |   |   ├── Debug
          |   |   |   └── anpr.exe
          |   |   └── Release
          |   |       └── anpr.exe
          |   ├── mingw-x86-64
          |   |   └── anpr.exe
          |   ├── mingw-x86
          |   |   └── anpr.exe
          |   ├── linux-x86-64
          |   |   └── anpr
          |   └── linux-aarch64
          |       └── anpr
          └── build             # temporary build directory
  ```

### 2. How to Build and Run

1. Windows 64bit (MSVC, x64 Native Tools Command Prompt)

   ```sh
   mkdir build\windows-x86_64
   cd build\windows-x86_64

   # Install vcpkg
   git clone https://github.com/microsoft/vcpkg.git
   cd vcpkg
   bootstrap-vcpkg.bat

   # Install libpng and libjpeg
   vcpkg install libpng:x64-windows libjpeg-turbo:x64-windows
   cd ..

   cmake ../.. -G "Visual Studio 17 2022" -A x64 -DCMAKE_TOOLCHAIN_FILE=vcpkg/scripts/buildsystems/vcpkg.cmake
   cmake --build . --config Debug   # Debug build
   cmake --build . --config Release # Release build
   cd ..\..

   # Run this if non-ASCII characters appear garbled on Windows.
   chcp 65001

   # Run
   bin\windows-x86_64\Debug\anpr.exe
   bin\windows-x86_64\Release\anpr.exe
   ```

2. Windows 32bit (MSVC, x86 Native Tools Command Prompt)

   ```sh
   mkdir build\windows-x86
   cd build\windows-x86

   # Install vcpkg
   git clone https://github.com/microsoft/vcpkg.git
   cd vcpkg
   bootstrap-vcpkg.bat

   # Install libpng and libjpeg
   vcpkg install libpng:x86-windows libjpeg-turbo:x86-windows
   cd ..

   cmake ../.. -G "Visual Studio 17 2022" -A Win32  -DCMAKE_TOOLCHAIN_FILE=vcpkg/scripts/buildsystems/vcpkg.cmake
   cmake --build . --config Debug   # Debug build
   cmake --build . --config Release # Release build
   cd ..\..

   # Run this if non-ASCII characters appear garbled on Windows.
   chcp 65001

   # Run
   bin\windows-x86\Debug\anpr.exe
   bin\windows-x86\Release\anpr.exe
   ```

3. Windows MinGW64 (MSYS2)

   ```sh
   # install dependency
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
   # install dependency
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
   # install dependency
   # Debian / Ubuntu Linux
   sudo apt install build-essential libpng-dev libjpeg-dev

   # Oracle Linux / RedHat (RHEL) / CentOS
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
   # install dependency
   # Debian / Ubuntu Linux
   sudo apt install build-essential libpng-dev libjpeg-dev

   # Oracle Linux / RedHat (RHEL) / CentOS
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
