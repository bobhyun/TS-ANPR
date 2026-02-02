[English](../../README.md) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# Odin 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Odin/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Odin/anpr)

### 1. 엔진 파일 복사

_**[참고]** 이 예제에서는 다른 예제들과 공유하기 위해 엔진 파일을 examples/bin/ 디렉토리에 압축 해제합니다. 하지만 실제 배포 시에는 일반적으로 애플리케이션의 실행 파일이 위치한 디렉토리에 엔진 파일을 복사합니다._

- Windows x86 64비트용
  엔진 파일을 `examples/bin/windows-x86_64` 디렉토리에 압축 해제
  ```sh
  7z x tsanpr*-windows-x86_64.7z
  ```
- Windows x86 32비트용
  엔진 파일을 `examples/bin/windows-x86` 디렉토리에 압축 해제
  ```sh
  7z x tsanpr*-windows-x86.7z
  ```
- Linux x86 64비트용
  엔진 파일을 `examples/bin/linux-x86_64` 디렉토리에 압축 해제
  ```sh
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- Linux arm 64비트용
  엔진 파일을 `examples/bin/linux-aarch64` 디렉토리에 압축 해제
  ```sh
  tar xvf tsanpr*-linux-aarch64.tar.xz
  ```
- 디렉토리 구조
  ```sh
  examples
  ├── bin
  │   ├─── windows-x86_64        # Windows (x86_64)용 엔진 디렉토리
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86           # Windows (x86)용 엔진 디렉토리
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64           # Linux (x86_64)용 엔진 디렉토리
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # Linux (arm64)용 엔진 디렉토리
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # 이미지 디렉토리
  └── Odin
      └── anpr                   # 프로젝트 디렉토리
          ├── anpr.odin
          ├── tsanpr             # tsanpr 패키지 디렉토리
          │   └── tsanpr.odin
          ├── stb_image          # stb_image 패키지 디렉토리
          │   ├── stb_image.odin
          │   ├── stb_image.h
          │   └── stb_image_impl.c
          ├── build.bat
          └── build.sh
  ```

### 2. 실행 방법

1. Odin 설치

   ```sh
   # Windows (Scoop 사용)
   scoop install odin

   # macOS (Homebrew 사용)
   brew install odin

   # Linux (Arch Linux)
   pacman -S odin

   # Linux (Ubuntu/Debian - 소스에서 빌드)
   sudo apt install llvm clang
   git clone https://github.com/odin-lang/Odin.git
   cd Odin && make release
   echo 'export PATH=$PATH:$HOME/Odin' >> ~/.bashrc && source ~/.bashrc
   ```

2. `anpr` 빌드 및 실행

   빌드 스크립트는 stb_image C 라이브러리를 컴파일한 후 Odin 프로젝트를 빌드합니다.

   ```sh
   cd examples/Odin/anpr

   # Windows (Visual Studio 또는 MinGW 필요)
   build.bat
   anpr.exe

   # Linux/macOS (cc/gcc/clang 필요)
   chmod +x build.sh
   ./build.sh
   ./anpr
   ```
