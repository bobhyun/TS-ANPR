[English](../README.md) | [한국어](README_ko-KR.md) | [日本語](README_ja-JP.md) | Tiếng Việt

# Ví dụ Scala

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
  └── Scala
      └── anpr
          ├── CMakeLists.txt
          ├── toolchain-aarch64.cmake
          ├── pom.xml
          ├── src
          │   ├── main
          │   │   └── scala
          │   │       └── com
          │   │           └── example
          │   │               └── anpr
          │   │                   ├── Main.scala
          │   │                   └── TSANPR.scala
          │   └── native
          │       ├── TSANPR_jni.cpp
          │       ├── com_example_anpr_TSANPR.h
          │       ├── tsanpr.cpp
          │       └── tsanpr.h
          ├── bin               # compiled JNI modules
          │   ├── linux-aarch64
          │   │   └── jni
          │   │       └── libtsanpr_jni.so
          │   ├── linux-x86_64
          │   │   └── jni
          │   │       └── libtsanpr_jni.so
          │   ├── windows-x86
          │   │   └── jni
          │   │       ├── Debug
          │   │       │   └── tsanpr_jni.dll
          │   │       └── Release
          │   │           └── tsanpr_jni.dll
          │   └── windows-x86_64
          │       └── jni
          │           ├── Debug
          │           │   └── tsanpr_jni.dll
          │           └── Release
          │               └── tsanpr_jni.dll
          ├── build             # temporary build directory
          └── target            # compiled Java modules
              └── classes
                  └── com
                      └── example
                          └── anpr
                              ├── Main.class
                              └── TSANPR.class
  ```

### 2. Cách biên dịch

1. Native(JNI) Module

   - Windows 64bit (MSVC, x64 Native Tools Command Prompt)

     ```sh
     mkdir build\windows-x86_64
     cd build\windows-x86_64

     cmake ..\.. -A x64

     cmake --build . --config Debug   # Debug build
     cmake --build . --config Release # Release build
     ```

   - Windows 32bit (MSVC, x86 Native Tools Command Prompt)

     ```sh
     mkdir build\windows-x86
     cd build\windows-x86

     cmake ..\.. -A Win32

     cmake --build . --config Debug   # Debug build
     cmake --build . --config Release # Release build
     ```

   - Linux x86_64

     ```sh
     mkdir -p build/linux-x86_64
     cd build/linux-x86_64

     # Debug build
     cmake ../.. -DCMAKE_BUILD_TYPE=Debug
     make

     # Release build
     cmake ../.. -DCMAKE_BUILD_TYPE=Release
     make
     ```

   - Linux aarch64 (ARM64)

     - Biên dịch trực tiếp trên máy aarch64

       ```sh
       mkdir -p build/linux-aarch64
       cd build/linux-aarch64

       # Debug build
       cmake ../.. -DCMAKE_BUILD_TYPE=Debug
       make

       # Release build
       cmake ../.. -DCMAKE_BUILD_TYPE=Release
       make
       ```

     - Biên dịch chéo trên x86_64

       ```sh
       mkdir build/linux-aarch64
       cd build/linux-aarch64

       # Debug build
       cmake ../.. -DCMAKE_TOOLCHAIN_FILE=../../toolchain-aarch64.cmake -DCMAKE_BUILD_TYPE=Debug
       make

       # Release build
       cmake ../.. -DCMAKE_TOOLCHAIN_FILE=../../toolchain-aarch64.cmake -DCMAKE_BUILD_TYPE=Release
       make
       ```

2. Scala Module

   ```sh
   mvn clean package
   ```

### 3. Cách chạy

```sh
# Nếu các ký tự không phải ASCII bị hiển thị sai trên Windows, hãy chạy lệnh này.
chcp 65001

mvn scala:run -DmainClass=com.example.anpr.Main
```
