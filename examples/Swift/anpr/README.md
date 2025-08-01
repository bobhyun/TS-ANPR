English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# Swift Example

https://github.com/bobhyun/TS-ANPR/tree/main/examples/Swift/anpr

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

### 2. How to Build and Run

1. Windows 64bit (MSVC, x64 Native Tools Command Prompt)

   ```sh
   mkdir build\windows-x86_64
   cd build\windows-x86_64

   # Install vcpkg
   git clone https://github.com/microsoft/vcpkg.git
   cd vcpkg
   bootstrap-vcpkg.bat

   # Install opencv
   vcpkg install opencv:x64-windows
   cd ..

   cmake ../.. -G "Visual Studio 17 2022" -A x64 -DCMAKE_TOOLCHAIN_FILE=vcpkg/scripts/buildsystems/vcpkg.cmake
   cmake --build .                  # Debug build
   cmake --build . --config Release # Release build
   cd ..\..
   swift build             # Debug build
   swift build -c release  # Release build

   # Run this if non-ASCII characters appear garbled on Windows.
   chcp 65001

   # Run
   .build\x86_64-unknown-windows-msvc\debug\anpr.exe
   .build\x86_64-unknown-windows-msvc\release\anpr.exe
   ```

2. Linux x86_64

   ```sh
   # install dependency
   # Debian / Ubuntu Linux
   sudo apt update
   sudo apt install libopencv-dev

   # Oracle Linux / RedHat (RHEL) / CentOS
   sudo yum install epel-release
   sudo yum install opencv-devel

   mkdir -p build/linux-x86_64
   cd build/linux-x86_64

   # Debug build
   cmake ../.. -DCMAKE_BUILD_TYPE=Debug
   make

   # Release build
   cmake ../.. -DCMAKE_BUILD_TYPE=Release
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
   # install dependency
   # Debian / Ubuntu Linux
   sudo apt update
   sudo apt install libopencv-dev

   # Oracle Linux / RedHat (RHEL) / CentOS
   sudo yum install epel-release
   sudo yum install opencv-devel

   mkdir -p build/linux-aarch64
   cd build/linux-aarch64

   # Debug build
   cmake ../.. -DCMAKE_BUILD_TYPE=Debug
   make

   # Release build
   cmake ../.. -DCMAKE_BUILD_TYPE=Release
   make
   cd ../..

   swift build             # Debug build
   swift build -c release  # Release build

   # Run
   .build/aarch64-unknown-linux-gnu/debug/anpr
   .build/aarch64-unknown-linux-gnu/release/anpr
   ```
