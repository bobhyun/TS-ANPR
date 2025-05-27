[English](../README.md) | [日本語](README_ja-JP.md) | 한국어 | [Tiếng Việt](README_vi-VN.md)

# Go 예제

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
  │   ├─── windows-x86_64        # engine directory for windows (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   ├── linux-x86_64           # engine directory for linux (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # engine directory for linux (arm64)
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # image directory
  └── Go
      └── anpr                   # source directory
         ├── anpr                # compiled executable for Windows
         ├── anpr.exe            # compiled executable for Linux
         ├── go.mod
         ├── main.go
         └── tsanpr
            ├── tsanpr_windows.go
            └── tsanpr_linux.go
  ```

### 2. 빌드 및 실행

#### 2.1 윈도우즈

1. 모듈 초기화

   ```sh
   go mod tidy
   ```

2. 종속성 설치

   - GoCV 공식 설치 가이드 참고 (https://gocv.io/getting-started/windows/)

3. 빌드 방법

   ```sh
   go build -o anpr.exe main.go
   ```

4. 실행 방법

   ```sh
   .\anpr.exe
   ```

#### 2.2. 리눅스

1. 모듈 초기화

   ```sh
   go mod tidy
   ```

2. 종속성 설치

   - Debian / Ubuntu Linux

     ```sh
     sudo apt install -y build-essential g++ libopencv-dev
     ```

   - Oracle Linux / RedHat (RHEL) / CentOS

     ```sh
     sudo def install -y epel-release gcc gcc-c++ opencv opencv-devel
     ```

3. 빌드 방법

   ```sh
   go build -o anpr main.go
   ```

4. 실행 방법

   ```sh
   ./anpr
   ```
