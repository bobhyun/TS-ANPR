[English](../../) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# C++ サンプル

https://github.com/bobhyun/TS-ANPR/tree/main/examples/C%2B%2B/anpr

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
  └── C++
      └── anpr                  # source directory
          ├── CMakeLists.txt
          ├── toolchain-aarch64.cmake
          ├── src
          │   ├── tsanpr.h
          │   ├── tsanpr.cpp
          │   └── anpr.cpp
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

   cmake ../.. -G "Visual Studio 17 2022" -A x64 -DCMAKE_TOOLCHAIN_FILE=vcpkg/scripts/buildsystems/vcpkg.cmake
   cmake --build . --config Debug   # Debug build
   cmake --build . --config Release # Release build
   cd ..\..

   # Windowsで非ASCII文字が文字化けする場合は、これを実行してください。
   chcp 65001

   # Run
   bin\windows-x86_64\Debug\anpr.exe
   bin\windows-x86_64\Release\anpr.exe
   ```

2. Windows 32bit (MSVC, x86 Native Tools Command Prompt)

   ```sh
   mkdir build\windows-x86
   cd build\windows-x86

   # vcpkgのインストール
   git clone https://github.com/microsoft/vcpkg.git
   cd vcpkg
   bootstrap-vcpkg.bat

   # opencv のインストール
   vcpkg install opencv:x86-windows
   cd ..

   cmake ../.. -G "Visual Studio 17 2022" -A Win32  -DCMAKE_TOOLCHAIN_FILE=vcpkg/scripts/buildsystems/vcpkg.cmake
   cmake --build . --config Debug   # Debug build
   cmake --build . --config Release # Release build
   cd ..\..

   # Windowsで非ASCII文字が文字化けする場合は、これを実行してください。
   chcp 65001

   # Run
   bin\windows-x86\Debug\anpr.exe
   bin\windows-x86\Release\anpr.exe
   ```

3. Windows MinGW64 (MSYS2)

   ```sh
   # 依存関係をインストールする
   pacman -Syu
   pacman -S mingw-w64-x86_64-gcc mingw-w64-x86_64-g++ mingw-w64-x86_64-opencv

   mkdir -p build/mingw-x86_64
   cd build/mingw-x86_64

   # build
   cmake ../.. -G "MinGW Makefiles"
   mingw32-make
   cd ../..

   # Run
   bin/mingw-x86_64/anpr
   ```

4. Windows MinGW32 (MSYS2)

   ```sh
   # 依存関係をインストールする
   pacman -Syu
   pacman -S mingw-w64-i686-gcc mingw-w64-i686-g++ mingw-w64-i686-opencv

   mkdir -p build/mingw-x86
   cd build/mingw-x86

   # build
   cmake ../.. -G "MinGW Makefiles"
   mingw32-make
   cd ../..

   # Run
   bin/mingw-x86/anpr
   ```

5. Linux x86_64

   ```sh
   # 依存関係をインストールする
   # Debian / Ubuntu Linux
   sudo apt install build-essential libopencv-dev

   # Oracle Linux / RedHat (RHEL) / CentOS
   sudo yum install epel-release
   sudo yum install gcc g++ make opencv opencv-devel

   mkdir -p build/linux-x86_64
   cd build/linux-x86_64

   # Debug build
   cmake ../.. -DCMAKE_BUILD_TYPE=Debug
   make

   # Release build
   cmake ../.. -DCMAKE_BUILD_TYPE=Release
   make
   cd ../..

   # Run
   bin/linux-x86_64/anpr
   ```

6. Linux aarch64 (ARM64)

   ```sh
   # 依存関係をインストールする
   # Debian / Ubuntu Linux
   sudo apt install build-essential libopencv-dev

   # Oracle Linux / RedHat (RHEL) / CentOS
   sudo yum install epel-release
   sudo yum install gcc g++ make opencv opencv-devel

   mkdir -p build/linux-aarch64
   cd build/linux-aarch64

   # Debug build
   cmake ../.. -DCMAKE_BUILD_TYPE=Debug
   make

   # Release build
   cmake ../.. -DCMAKE_BUILD_TYPE=Release
   make
   cd ../..

   # Run
   bin/linux-aarch64/anpr
   ```
