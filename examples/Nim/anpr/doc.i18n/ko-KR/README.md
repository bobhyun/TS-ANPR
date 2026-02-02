[English](../../README.md) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# Nim 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Nim/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Nim/anpr)

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
  └── Nim
      └── anpr                   # 프로젝트 디렉토리
          ├── anpr.nimble
          └── src                # 소스 디렉토리
              ├── anpr.nim
              ├── tsanpr.nim
              ├── stb_image.h
              └── stb_image_impl.c
  ```

### 2. 실행 방법

1. Nim 설치

   ```sh
   # Windows (Scoop 사용)
   scoop install nim

   # Linux (Ubuntu/Debian)
   sudo apt-get install nim

   # 또는 choosenim 사용 (권장)
   curl https://nim-lang.org/choosenim/init.sh -sSf | sh
   ```

2. `anpr` 실행

   ```sh
   cd examples/Nim/anpr
   nimble run anpr
   ```
