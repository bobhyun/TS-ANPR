[English](../../) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# Swift サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Swift/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Swift/anpr)

### 1. エンジンファイルのコピー

_**【参考】** この例では、他のサンプルとエンジンファイルを共有するために `examples/bin/` ディレクトリに展開しますが、実際の配布時には通常、アプリケーションの実行ファイルがあるディレクトリにエンジンファイルをコピーします。_

- Windows x86 64 ビット
  エンジンファイルを `examples/bin/windows-x86_64` ディレクトリに展開
  ```sh
  unzip tsanpr*-windows-x86_64.zip
  ```
- Windows x86 32 ビット
  エンジンファイルを `examples/bin/windows-x86` ディレクトリに展開
  ```sh
  unzip tsanpr*-windows-x86.zip
  ```
- Linux x86 64 ビット
  エンジンファイルを `examples/bin/linux-x86_64` ディレクトリに展開
  ```sh
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- Linux ARM 64 ビット
  エンジンファイルを `examples/bin/linux-aarch64` ディレクトリに展開
  ```sh
  tar xvf tsanpr*-linux-aarch64.tar.xz
  ```
- ディレクトリ構成
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

### 2. ビルドおよび実行方法

1. Windows 64bit (MSVC, x64 Native Tools Command Prompt)

   ```sh
   mkdir build\windows-x86_64
   cd build\windows-x86_64

   # vcpkgのインストール
   git clone https://github.com/microsoft/vcpkg.git
   cd vcpkg
   bootstrap-vcpkg.bat

   # opencv のインストール
   vcpkg install opencv:x64-windows
   cd ..

   cmake ../../CBridge -G "Visual Studio 17 2022" -A x64 -DCMAKE_TOOLCHAIN_FILE=vcpkg/scripts/buildsystems/vcpkg.cmake
   cmake --build . --config Debug   # Debug build
   cmake --build . --config Release # Release build
   cd ..\..
   swift build             # Debug build
   swift build -c release  # Release build

   # Windowsで非ASCII文字が文字化けする場合は、これを実行してください。
   chcp 65001

   # Run
   .build\x86_64-unknown-windows-msvc\debug\anpr.exe
   .build\x86_64-unknown-windows-msvc\release\anpr.exe
   ```

2. Linux x86_64

   ```sh
   # 依存関係をインストールする
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
   # 依存関係をインストールする
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
