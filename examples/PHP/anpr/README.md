English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# PHP Example

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/PHP/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/PHP/anpr)

### 1. Copying the Engine Files

_**[Note]** In this example, the engine file is extracted to the examples/bin/ directory to share it with other examples. However, for actual deployment, the engine file is typically copied to the directory where the application's executable file is located._

- For Windows x86 64-bit
  Extract the engine file to the `examples/bin/windows-x86_64` directory
  ```sh
  7z x tsanpr*-windows-x86_64.7z
  ```
- For Windows x86 32-bit
  Extract the engine file to the `examples/bin/windows-x86` directory
  ```sh
  7z x tsanpr*-windows-x86.7z
  ```
- For Linux x86 64-bit
  Extract the engine file to the `examples/bin/linux-x86_64` directory
  ```sh
  tar xvf tsanpr-linux-x86_64.tar.xz
  ```
- For Linux arm 64-bit
  Extract the engine file to the `examples/bin/linux-aarch64` directory
  ```sh
  tar xvf tsanpr-linux-aarch64.tar.xz
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
  ├── img                        # image directory
  └── PHP
      └── anpr                   # source directory
         ├── anpr.php
         ├── TSANPR.php
         └── composer.json
  ```

### 2. Prerequisites

1. Install PHP 8.0 or higher with required extensions

   **Ubuntu/Debian:**

   ```sh
   # Add ondrej/php PPA for PHP packages
   sudo apt-get update
   sudo apt-get install -y software-properties-common
   sudo add-apt-repository -y ppa:ondrej/php
   sudo apt-get update

   # Install PHP with required extensions
   sudo apt-get install -y php8.2 php8.2-cli php8.2-imagick

   # Enable FFI in php.ini
   sudo sed -i 's/;ffi.enable.*/ffi.enable=true/' /etc/php/8.2/cli/php.ini
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   sudo dnf install php php-cli php-pecl-imagick

   # Enable FFI in php.ini
   sudo sed -i 's/;ffi.enable.*/ffi.enable=true/' /etc/php.ini
   ```

   **macOS:**

   ```sh
   brew install php imagemagick
   pecl install imagick

   # Enable FFI in php.ini
   echo "ffi.enable=true" >> $(php --ini | grep "Loaded Configuration" | sed -e "s|.*:\s*||")
   ```

   **Windows:**

   - Download PHP from https://windows.php.net/download/
   - Install ImageMagick and php_imagick.dll
   - Enable in php.ini:
     ```ini
     ffi.enable=true
     extension=imagick
     ```

2. Verify installation

   ```sh
   php --version
   php -m | grep -E "(FFI|imagick)"
   ```

### 3. How to Run

```sh
cd examples/PHP/anpr
php anpr.php
```

### 4. Features

- **readImageFile**: Process image files directly using `anpr_read_file()`
- **readEncodedImage**: Process encoded image data (JPEG, PNG, etc.) using `anpr_read_pixels()` with 'encoded' format
- **readPixelBuffer**: Process raw pixel data using Imagick extension

### 5. API Reference

#### TSANPR Class

```php
class TSANPR {
    public function __construct(string $libraryPath);
    public function anprInitialize(string $mode): string;
    public function anprReadFile(string $imgFileName, string $outputFormat, string $options): string;
    public function anprReadPixels(string $pixels, int $width, int $height, int $stride,
                                   string $pixelFormat, string $outputFormat, string $options): string;
}
```

#### Recognition Options

| Option | Description |
|--------|-------------|
| `""` | Single license plate recognition (default) |
| `"vm"` | Recognize multiple license plates attached to vehicles |
| `"vmb"` | Recognize multiple license plates (including motorcycles) |
| `"vms"` | Recognize with surround detection |
| `"dms"` | Detect multiple surrounding objects (vehicles) |
| `"dmsr"` | Detect objects and recognize license plates |
| `"dmsri<coords>"` | Recognize within Region of Interest |

#### Output Formats

`"text"`, `"json"`, `"yaml"`, `"xml"`, `"csv"`

### 6. Troubleshooting

**FFI not loaded:**
```sh
# Check FFI status
php -i | grep -i ffi

# Enable FFI in php.ini
ffi.enable=true
```

**Imagick not available:**
```sh
# Ubuntu/Debian
sudo apt-get install php8.2-imagick

# Verify
php -m | grep imagick
```
