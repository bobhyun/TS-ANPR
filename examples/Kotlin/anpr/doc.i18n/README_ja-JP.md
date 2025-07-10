[English](../README.md) | [한국어](README_ko-KR.md) | 日本語 | [Tiếng Việt](README_vi-VN.md)

# Kotlin サンプル

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
  └── Kotlin
      └── anpr
          ├── CMakeLists.txt
          ├── toolchain-aarch64.cmake
          ├── pom.xml
          ├── src
          │   ├── main
          │   │   ├── java
          │   │       └── com
          │   │           └── example
          │   │               └── anpr
          │   │                   ├── Main.kt
          │   │                   └── TSANPR.kt
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
          └── target            # compiled Kotlin modules
              └── classes
                  └── com
                      └── example
                          └── anpr
                              ├── AnprFunction.class
                              ├── Main.class
                              └── TSANPR.class
  ```

### 2. How to Build

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

     - aarch64 マシンで直接ビルド

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

     - x86_64 でクロスコンパイル

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

2. Kotlin Module

   ```sh
   cd src
   kotlinc main/java/com/example/anpr/Main.kt main/java/com/example/anpr/TSANPR.kt -d ../target/classes
   cd ..
   ```

### 3. 実行方法

```sh
# Windowsで非ASCII文字が文字化けする場合は、これを実行してください。
chcp 65001

java -cp target/classes com.example.anpr.Main
```
