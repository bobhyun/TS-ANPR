English | [日本語](doc.i18n/README_ja-JP.md) | [한국어](doc.i18n/README_ko-KR.md) | [Tiếng Việt](doc.i18n/README_vi-VN.md)

# Kotlin Example

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

     - Build directly on the aarch64 machine

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

     - Cross-compile on x86_64

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

### 3. How to Run

```sh
# Run this if non-ASCII characters appear garbled on Windows.
chcp 65001

java -cp target/classes com.example.anpr.Main
```
