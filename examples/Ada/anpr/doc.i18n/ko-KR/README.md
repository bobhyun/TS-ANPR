[English](../../README.md) | 한국어 | [日本語](../ja-JP/README.md) | [Tiếng Việt](../vi-VN/README.md)

# Ada 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Ada/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Ada/anpr)

### 1. 엔진 파일 복사

_**[참고]** 이 예제의 경우 다른 예제들과 엔진 파일을 공유하기 위해 `examples/bin/` 디렉토리에 압축 해제하지만 실제 배포시는 일반적으로 응용 프로그램의 실행 파일이 있는 디렉토리에 엔진 파일을 복사합니다._

- 윈도우즈 x86 64비트
  엔진 파일을 `examples/bin/windows-x86_64` 디렉토리에 압축 해제
  ```sh
  tar xvf tsanpr*-windows-x86_64.tar.xz
  ```
- 윈도우즈 x86 32비트
  엔진 파일을 `examples/bin/windows-x86` 디렉토리에 압축 해제
  ```sh
  tar xvf tsanpr*-windows-x86.tar.xz
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
  │   ├─── windows-x86_64        # Windows (x86_64) 엔진 디렉토리
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86           # Windows (x86) 엔진 디렉토리
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64           # Linux (x86_64) 엔진 디렉토리
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # Linux (arm64) 엔진 디렉토리
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # 이미지 디렉토리
  └── Ada
      └── anpr                   # 프로젝트 디렉토리
         ├── bin                 # 실행 파일 디렉토리
         ├── obj                 # 오브젝트 파일 디렉토리
         ├── src                 # 소스 디렉토리
         │   ├── anpr.adb
         │   ├── tsanpr.ads
         │   ├── tsanpr-windows.adb
         │   └── tsanpr-unix.adb
         ├── anpr.gpr
         ├── alire.toml
         ├── compile.bat
         ├── compile.sh
         └── Makefile
  ```

### 2. 빌드 및 실행

#### 2.1 Alire 사용 (권장)

[Alire](https://alire.ada.dev/)는 Ada의 현대적인 패키지 매니저로, 툴체인과 의존성을 자동으로 관리합니다.

1. Alire 설치

   **Windows:**

   - [https://alire.ada.dev](https://alire.ada.dev)에서 다운로드
   - 압축 해제 후 `alr`을 PATH에 추가

   **Linux:**

   **권장: 수동 설치 (모든 배포판)**

   [Alire Releases](https://github.com/alire-project/alire/releases)에서 최신 버전 다운로드:

   ```sh
   # 최신 릴리스 다운로드 (현재 버전은 릴리스 페이지에서 확인)
   wget https://github.com/alire-project/alire/releases/download/v2.1.0/alr-2.1.0-bin-x86_64-linux.zip
   unzip alr-2.1.0-bin-x86_64-linux.zip
   sudo mv bin/alr /usr/local/bin/
   ```

   또는 [https://alire.ada.dev](https://alire.ada.dev)에서 다운로드 후 PATH에 추가

   **대안: 패키지 관리자 사용 (구 버전일 수 있음)**

   - Debian/Ubuntu:
     ```sh
     sudo apt-get update
     sudo apt-get install alire
     ```
   - Fedora/RHEL/CentOS:
     ```sh
     sudo dnf install alire
     ```

   **참고:** 패키지 관리자로 설치한 버전에서 `Unexpected property count: 0` 오류가 발생하면, 위의 수동 설치 방법으로 최신 버전을 설치하세요.

2. 빌드

   ```sh
   alr build
   ```

   첫 빌드 시 Alire가 자동으로 GNAT 컴파일러와 GPRbuild를 다운로드합니다. 메시지가 나타나면 Enter를 누르세요.

3. 실행

   ```sh
   alr run
   ```

   또는 직접 실행:

   ```sh
   bin/anpr
   ```

#### 2.2 GNAT/GPRbuild 사용 (전통적 방법)

##### 2.2.1 Windows

1. GNAT 설치

   - [GNAT Community](https://www.adacore.com/download) 다운로드 및 설치
   - PATH에 GNAT bin 디렉토리 추가

2. 빌드 방법

   ```cmd
   compile.bat
   ```

   또는 GPRbuild 사용:

   ```cmd
   gprbuild -p -P anpr.gpr -XOS=Windows_NT
   ```

3. 실행 방법

   UTF-8 문자 표시를 위해 (한글 등 non-ASCII 문자 권장):

   ```cmd
   chcp 65001
   bin\anpr.exe
   ```

   또는 UTF-8 인코딩 없이:

   ```cmd
   bin\anpr.exe
   ```

##### 2.2.2 Linux

1. 종속성 설치

   - Debian / Ubuntu Linux

     ```sh
     sudo apt-get update
     sudo apt-get install gnat gprbuild
     ```

   - Oracle Linux / RedHat (RHEL) / CentOS

     ```sh
     sudo yum install gcc-gnat gprbuild
     ```

   - Fedora

     ```sh
     sudo dnf install gcc-gnat gprbuild
     ```

2. 빌드 방법

   ```sh
   chmod +x compile.sh
   ./compile.sh
   ```

   또는 GPRbuild 사용:

   ```sh
   gprbuild -p -P anpr.gpr -XOS=UNIX
   ```

   또는 Make 사용:

   ```sh
   make
   ```

3. 실행 방법

   ```sh
   bin/anpr
   ```

   **문자 인코딩 참고:**
   Linux 시스템은 기본적으로 UTF-8을 지원하므로 한글이 정상적으로 표시됩니다. 문제가 있다면 터미널이 UTF-8 인코딩으로 설정되어 있는지 확인하세요.
