[English](../../) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ Swift

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Swift/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Swift/anpr)

### 1. Sao chép tệp Engine

_**[Lưu ý]** Trong ví dụ này, tệp engine được giải nén vào thư mục examples/bin/ để chia sẻ với các ví dụ khác. Tuy nhiên, khi triển khai thực tế, tệp engine thường được sao chép vào thư mục chứa tệp thực thi của ứng dụng._

- Windows x86 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86_64`
  ```sh
  7z x tsanpr*-windows-x86_64.7z
  ```
- Windows x86 32-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86`
  ```sh
  7z x tsanpr*-windows-x86.7z
  ```
- Linux x86 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/linux-x86_64`
  ```sh
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- Linux ARM 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/linux-aarch64`
  ```sh
  tar xvf tsanpr*-linux-aarch64.tar.xz
  ```
- Cấu trúc thư mục
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

### 2. Cách xây dựng và chạy

1. Windows 64bit (MSVC, x64 Native Tools Command Prompt)

   ```sh
   mkdir build\windows-x86_64
   cd build\windows-x86_64

   # Cài đặt vcpkg
   git clone https://github.com/microsoft/vcpkg.git
   cd vcpkg
   bootstrap-vcpkg.bat

   # Cài đặt opencv
   vcpkg install opencv:x64-windows
   cd ..

   cmake ../../CBridge -G "Visual Studio 17 2022" -A x64 -DCMAKE_TOOLCHAIN_FILE=vcpkg/scripts/buildsystems/vcpkg.cmake
   cmake --build . --config Debug   # Debug build
   cmake --build . --config Release # Release build
   cd ..\..
   swift build             # Debug build
   swift build -c release  # Release build

   # Nếu các ký tự không phải ASCII bị hiển thị sai trên Windows, hãy  chạy lệnh này.
   chcp 65001

   # Run
   .build\x86_64-unknown-windows-msvc\debug\anpr.exe
   .build\x86_64-unknown-windows-msvc\release\anpr.exe
   ```

1. Linux x86_64

   ```sh
   # Cài đặt phụ thuộc
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

1. Linux aarch64 (ARM64)

   ```sh
   # Cài đặt phụ thuộc
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
