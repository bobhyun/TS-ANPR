[English](../../README.md) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# Groovy 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Groovy/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Groovy/anpr)

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
  tar xvf tsanpr-linux-x86_64.tar.xz
  ```
- Linux arm 64비트용
  엔진 파일을 `examples/bin/linux-aarch64` 디렉토리에 압축 해제
  ```sh
  tar xvf tsanpr-linux-aarch64.tar.xz
  ```
- 디렉토리 구조
  ```
  examples/
  ├── bin/
  │   ├── windows-x86_64/       # Windows (x86_64)용 엔진
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── windows-x86/          # Windows (x86)용 엔진
  │   │   └── ...
  │   ├── linux-x86_64/         # Linux (x86_64)용 엔진
  │   │   └── ...
  │   └── linux-aarch64/        # Linux (arm64)용 엔진
  │       └── ...
  ├── img/
  └── Groovy/
      └── anpr/                 # Groovy 소스 디렉토리
          ├── anpr.groovy       # 메인 예제
          └── TSANPR.groovy     # TSANPR JNA 래퍼 모듈
  ```

### 2. 사전 요구사항

1. Java JDK 8 이상 설치

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get update
   sudo apt-get install -y openjdk-11-jdk
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   sudo dnf install java-11-openjdk-devel
   ```

   **macOS:**

   ```sh
   brew install openjdk@11
   ```

   **Windows:**

   - https://adoptium.net/ 또는 https://www.oracle.com/java/technologies/downloads/ 에서 JDK 다운로드 및 설치

2. Groovy 설치

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get install -y groovy
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   sudo dnf install groovy
   ```

   **macOS:**

   ```sh
   brew install groovy
   ```

   **Windows:**

   - https://groovy.apache.org/download.html 에서 다운로드
   - 또는 SDKMAN 사용: `sdk install groovy`

3. 설치 확인

   ```sh
   java -version
   groovy --version
   ```

### 3. 실행 방법

```sh
cd examples/Groovy/anpr

# ANPR 예제 실행
groovy anpr.groovy
```

### 4. 기능

- **readImageFile**: `anpr_read_file()`을 사용하여 이미지 파일 직접 처리
- **readEncodedImage**: 'encoded' 형식으로 `anpr_read_pixels()`를 사용하여 인코딩된 이미지 바이트 처리
- **readPixelBuffer**: Java AWT BufferedImage를 사용하여 원시 RGB 픽셀 데이터 처리
- **동적 로딩**: TSANPR 라이브러리는 JNA를 통해 런타임에 로드됨
- **크로스 플랫폼**: Windows 및 Linux (x86_64, x86, ARM64) 지원
- **자동 의존성**: JNA는 Grape (@Grab 어노테이션)를 통해 자동 다운로드됨

### 5. API 참조

#### TSANPR 모듈

`TSANPR` 클래스는 다음 메소드를 제공합니다:

```groovy
// TSANPR 라이브러리 로드
def tsanpr = new TSANPR(libraryPath)

// ANPR 엔진 초기화
String error = tsanpr.initialize(mode)

// 이미지 파일 읽기 및 처리
String result = tsanpr.readFile(imgFileName, outputFormat, options)

// 픽셀 데이터 직접 처리
String result = tsanpr.readPixels(pixels, width, height, stride, pixelFormat, outputFormat, options)
```

#### 인식 옵션

| 옵션 | 설명 |
|------|------|
| `""` | 단일 번호판 인식 (기본값) |
| `"vm"` | 차량에 부착된 여러 번호판 인식 |
| `"vmb"` | 여러 번호판 인식 (오토바이 포함) |
| `"vms"` | 서라운드 감지와 함께 인식 |
| `"dms"` | 여러 주변 객체 (차량) 감지 |
| `"dmsr"` | 객체 감지 및 번호판 인식 |
| `"dmsri<좌표>"` | 관심 영역 내 인식 |

#### 출력 형식

`"text"`, `"json"`, `"yaml"`, `"xml"`, `"csv"`

### 6. 구현 참고 사항

이 예제는 동적 라이브러리 로딩을 위해 JNA (Java Native Access)를 사용합니다:

**동적 라이브러리 로딩:**
- JNA는 JNI 보일러플레이트 없이 네이티브 공유 라이브러리에 쉽게 접근 제공
- `TSANPRLibrary` 인터페이스가 네이티브 함수 시그니처 정의
- `Native.load()` 메소드를 사용하여 라이브러리 로드

**Groovy 기능:**
- Grape (@Grab 어노테이션)를 사용한 자동 의존성 관리
- JNA 라이브러리는 첫 실행 시 자동 다운로드됨
- 유연한 함수 선택을 위한 클로저 및 메소드 참조

**픽셀 버퍼 처리:**
`readPixelBuffer` 함수는 이미지를 로드하고 디코딩하기 위해 Java의 `BufferedImage`를 사용합니다. 원시 픽셀 데이터를 추출하여 JNA의 `Memory` 클래스를 사용하여 적절한 픽셀 형식으로 `anpr_read_pixels()`에 전달합니다.

### 7. 문제 해결

**Groovy를 찾을 수 없음:**
```sh
# Groovy 설치 확인
groovy --version

# Ubuntu/Debian: Groovy 설치
sudo apt-get install groovy

# 또는 SDKMAN 사용
curl -s "https://get.sdkman.io" | bash
sdk install groovy
```

**JNA 의존성 문제:**
```sh
# Grape가 JNA를 자동으로 다운로드해야 함
# 문제가 지속되면 네트워크 연결 또는 프록시 설정 확인

# Grape 캐시 지우고 재시도
rm -rf ~/.groovy/grapes/net.java.dev.jna
groovy anpr.groovy
```

**라이브러리 로딩 문제:**
- TSANPR 라이브러리가 예상 위치에 있는지 확인
- Linux에서 필요시 `LD_LIBRARY_PATH` 확인
- 라이브러리 아키텍처가 JVM과 일치하는지 확인 (64비트 vs 32비트)

**런타임 문제:**
- 엔진 파일(`.eon`)이 라이브러리와 같은 디렉토리에 있는지 확인
- `tshelper`를 사용하여 라이선스가 설치되어 있는지 확인
