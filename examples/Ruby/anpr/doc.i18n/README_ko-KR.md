[English](../README.md) | 한국어 | [日本語](README_ja-JP.md) | [Tiếng Việt](README_vi-VN.md)

# Ruby 예제

### 1. 엔진 파일 복사

_**[참고]** 이 예제의 경우 다른 예제들과 엔진 파일을 공유하기 위해 `examples/bin/` 디렉토리에 압축 해제하지만 실제 배포시는 일반적으로 응용 프로그램의 실행 파일이 있는 디렉토리에 엔진 파일을 복사합니다._

- 윈도우즈 x86 64비트
  엔진 파일을 `examples/bin/windows-x86_64` 디렉토리에 압축 해제
  ```sh
  unzip tsanpr*-windows-x86_64.zip
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
  └── Ruby
      └── anpr                   # source directory
         ├── anpr.rb
         └── lib
            └── tsanpr.rb
  ```

### 2. 종속성 설치 및 실행

1. 종속성 설치

   1. Windows

      ```sh
      # Chocolatey 사용 권장
      choco install vips
      gem install ffi ruby-vips
      ```

   2. Linux

      ```sh
      # Debian / Ubuntu Linux
      sudo apt-get install -y libvips

      # Oracle Linux / RedHat (RHEL) / CentOS
      sudo yum install vips

      sudo gem install ffi ruby-vips
      ```

2. 실행 방법

   ```sh
   ruby anpr.rb
   ```
