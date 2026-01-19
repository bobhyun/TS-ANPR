[English](../../README.md) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# Mojo 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Mojo/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Mojo/anpr)

### 1. 엔진 파일 복사

_**[참고]** Mojo는 현재 Linux만 지원합니다. 이 예제의 경우 다른 예제들과 엔진 파일을 공유하기 위해 `examples/bin/` 디렉토리에 압축 해제하지만 실제 배포시는 일반적으로 응용 프로그램의 실행 파일이 있는 디렉토리에 엔진 파일을 복사합니다._

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
  │   ├── linux-x86_64           # 리눅스 (x86_64)용 엔진 디렉토리
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # 리눅스 (arm64)용 엔진 디렉토리
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # 이미지 디렉토리
  └── Mojo
      └── anpr                   # 프로젝트 디렉토리
          ├── pixi.toml
          └── src                # 소스 디렉토리
              ├── anpr.mojo
              ├── webcam.mojo
              └── tsanpr.mojo
  ```

### 2. 실행 방법

1. Mojo SDK 설치

   - pixi (패키지 관리자) 설치
     ```sh
     curl -fsSL https://pixi.sh/install.sh | sh
     source ~/.bashrc  # 또는 ~/.zshrc
     ```

   - pixi를 사용하여 Mojo 설치 (pixi.toml에 이미 설정됨)
     ```sh
     cd examples/Mojo/anpr
     pixi install
     ```

   자세한 내용은 https://docs.modular.com/mojo/manual/install/ 를 참조하세요.

2. `anpr` 실행

   ```sh
   pixi run anpr
   ```

3. `webcam` 실행

   ```sh
   pixi run webcam
   ```
