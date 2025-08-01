[English](../../) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ C

https://github.com/bobhyun/TS-ANPR/tree/main/examples/C/anpr

### 1. Sao chép tệp Engine

_**[Lưu ý]** Trong ví dụ này, tệp engine được giải nén vào thư mục examples/bin/ để chia sẻ với các ví dụ khác. Tuy nhiên, khi triển khai thực tế, tệp engine thường được sao chép vào thư mục chứa tệp thực thi của ứng dụng._

- Windows x86 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86_64`
  ```sh
  unzip tsanpr*-windows-x86_64.zip
  ```
- Windows x86 32-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86`
  ```sh
  unzip tsanpr*-windows-x86.zip
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

### 2. Cách xây dựng và chạy

1. Windows 64bit (MSVC, x64 Native Tools Command Prompt)

   ```sh
   mkdir build\windows-x86_64
   cd build\windows-x86_64

   # Cài đặt vcpkg
   git clone https://github.com/microsoft/vcpkg.git
   cd vcpkg
   bootstrap-vcpkg.bat

   # Cài đặt libpng và libjpeg
   vcpkg install libpng:x64-windows libjpeg-turbo:x64-windows
   cd ..

   cmake ../.. -G "Visual Studio 17 2022" -A x64 -DCMAKE_TOOLCHAIN_FILE=vcpkg/scripts/buildsystems/vcpkg.cmake
   cmake --build . --config Debug   # Debug build
   cmake --build . --config Release # Release build
   cd ..\..

   # Nếu các ký tự không phải ASCII bị hiển thị sai trên Windows, hãy  chạy lệnh này.
   chcp 65001

   # Run
   bin\windows-x86_64\Debug\anpr.exe
   bin\windows-x86_64\Release\anpr.exe
   ```

2. Windows 32bit (MSVC, x86 Native Tools Command Prompt)

   ```sh
   mkdir build\windows-x86
   cd build\windows-x86

   # Cài đặt vcpkg
   git clone https://github.com/microsoft/vcpkg.git
   cd vcpkg
   bootstrap-vcpkg.bat

   # Cài đặt libpng và libjpeg
   vcpkg install libpng:x86-windows libjpeg-turbo:x86-windows
   cd ..

   cmake ../.. -G "Visual Studio 17 2022" -A Win32 -DCMAKE_TOOLCHAIN_FILE=vcpkg/scripts/buildsystems/vcpkg.cmake
   cmake --build . --config Debug   # Debug build
   cmake --build . --config Release # Release build
   cd ..\..

   # Nếu các ký tự không phải ASCII bị hiển thị sai trên Windows, hãy  chạy lệnh này.
   chcp 65001

   # Run
   bin\windows-x86\Debug\anpr.exe
   bin\windows-x86\Release\anpr.exe
   ```

3. Windows MinGW64 (MSYS2)

   ```sh
   # Cài đặt phụ thuộc
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
   # Cài đặt phụ thuộc
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
   # Cài đặt phụ thuộc
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
   # Cài đặt phụ thuộc
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
