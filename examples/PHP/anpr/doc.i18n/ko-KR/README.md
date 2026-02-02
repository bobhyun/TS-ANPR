[English](../../README.md) | 한국어 | [日本語](../ja-JP/) | [Tiếng Việt](../vi-VN/)

# PHP 예제

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/PHP/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/PHP/anpr)

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
  └── PHP
      └── anpr                   # 소스 디렉토리
         ├── anpr.php
         ├── TSANPR.php
         └── composer.json
  ```

### 2. 사전 요구사항

1. PHP 8.0 이상 및 필수 확장 프로그램 설치

   **Ubuntu/Debian:**

   ```sh
   # ondrej/php PPA 추가
   sudo apt-get update
   sudo apt-get install -y software-properties-common
   sudo add-apt-repository -y ppa:ondrej/php
   sudo apt-get update

   # PHP 및 필수 확장 프로그램 설치
   sudo apt-get install -y php8.2 php8.2-cli php8.2-imagick

   # php.ini에서 FFI 활성화
   sudo sed -i 's/;ffi.enable.*/ffi.enable=true/' /etc/php/8.2/cli/php.ini
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   sudo dnf install php php-cli php-pecl-imagick

   # php.ini에서 FFI 활성화
   sudo sed -i 's/;ffi.enable.*/ffi.enable=true/' /etc/php.ini
   ```

   **macOS:**

   ```sh
   brew install php imagemagick
   pecl install imagick

   # php.ini에서 FFI 활성화
   echo "ffi.enable=true" >> $(php --ini | grep "Loaded Configuration" | sed -e "s|.*:\s*||")
   ```

   **Windows:**

   - https://windows.php.net/download/ 에서 PHP 다운로드
   - ImageMagick 및 php_imagick.dll 설치
   - php.ini에서 활성화:
     ```ini
     ffi.enable=true
     extension=imagick
     ```

2. 설치 확인

   ```sh
   php --version
   php -m | grep -E "(FFI|imagick)"
   ```

### 3. 실행 방법

```sh
cd examples/PHP/anpr
php anpr.php
```

### 4. 기능

- **readImageFile**: `anpr_read_file()`을 사용하여 이미지 파일 직접 처리
- **readEncodedImage**: `anpr_read_pixels()`의 'encoded' 형식으로 인코딩된 이미지 데이터(JPEG, PNG 등) 처리
- **readPixelBuffer**: Imagick 확장을 사용하여 원시 픽셀 데이터 처리

### 5. API 참조

#### TSANPR 클래스

```php
class TSANPR {
    public function __construct(string $libraryPath);
    public function anprInitialize(string $mode): string;
    public function anprReadFile(string $imgFileName, string $outputFormat, string $options): string;
    public function anprReadPixels(string $pixels, int $width, int $height, int $stride,
                                   string $pixelFormat, string $outputFormat, string $options): string;
}
```

#### 인식 옵션

| 옵션 | 설명 |
|------|------|
| `""` | 단일 번호판 인식 (기본값) |
| `"vm"` | 차량에 부착된 다중 번호판 인식 |
| `"vmb"` | 다중 번호판 인식 (오토바이 포함) |
| `"vms"` | 주변 감지와 함께 인식 |
| `"dms"` | 다중 주변 객체 (차량) 감지 |
| `"dmsr"` | 객체 감지 및 번호판 인식 |
| `"dmsri<좌표>"` | 관심 영역 내에서 인식 |

#### 출력 형식

`"text"`, `"json"`, `"yaml"`, `"xml"`, `"csv"`

### 6. 문제 해결

**FFI가 로드되지 않음:**
```sh
# FFI 상태 확인
php -i | grep -i ffi

# php.ini에서 FFI 활성화
ffi.enable=true
```

**Imagick을 사용할 수 없음:**
```sh
# Ubuntu/Debian
sudo apt-get install php8.2-imagick

# 확인
php -m | grep imagick
```
