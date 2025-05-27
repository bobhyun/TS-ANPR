[English](../README.md) | [日本語](README_ja-JP.md) | 한국어 | [Tiếng Việt](README_vi-VN.md)

# Java 예제

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
  └── Java
      └── anpr
          ├── CMakeLists.txt
          ├── toolchain-aarch64.cmake
          ├── pom.xml
          ├── src
          │   ├── main
          │   │   └── java
          │   │       └── com
          │   │           └── example
          │   │               └── anpr
          │   │                   ├── Main.java
          │   │                   └── TSANPR.java
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
                              ├── AnprFunction.class
                              ├── Main.class
                              └── TSANPR.class
  ```

### 2. 빌드 방법

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

     - aarch64 머신에서 직접 빌드

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

     - x86_64에서 크로스 컴파일

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

2. Java Module

   ```sh
   mvn clean package
   ```

### 3. 실행 방법

```sh
# 윈도우즈에서 non-ASCII 문자가 깨지는 경우 실행
chcp 65001

mvn exec:java -Dexec.mainClass=com.example.anpr.Main
```
