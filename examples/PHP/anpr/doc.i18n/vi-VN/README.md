[English](../../README.md) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ PHP

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/PHP/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/PHP/anpr)

### 1. Sao chép các tệp Engine

_**[Lưu ý]** Trong ví dụ này, tệp engine được giải nén vào thư mục examples/bin/ để chia sẻ với các ví dụ khác. Tuy nhiên, đối với triển khai thực tế, tệp engine thường được sao chép vào thư mục nơi tệp thực thi của ứng dụng được đặt._

- Cho Windows x86 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86_64`
  ```sh
  7z x tsanpr*-windows-x86_64.7z
  ```
- Cho Windows x86 32-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86`
  ```sh
  7z x tsanpr*-windows-x86.7z
  ```
- Cho Linux x86 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/linux-x86_64`
  ```sh
  tar xvf tsanpr-linux-x86_64.tar.xz
  ```
- Cho Linux arm 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/linux-aarch64`
  ```sh
  tar xvf tsanpr-linux-aarch64.tar.xz
  ```
- Cấu trúc thư mục
  ```sh
  examples
  ├── bin
  │   ├─── windows-x86_64        # thư mục engine cho Windows (x86_64)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86           # thư mục engine cho Windows (x86)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64           # thư mục engine cho Linux (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # thư mục engine cho Linux (arm64)
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # thư mục hình ảnh
  └── PHP
      └── anpr                   # thư mục nguồn
         ├── anpr.php
         ├── TSANPR.php
         └── composer.json
  ```

### 2. Yêu cầu tiên quyết

1. Cài đặt PHP 8.0 trở lên với các extension cần thiết

   **Ubuntu/Debian:**

   ```sh
   # Thêm PPA ondrej/php
   sudo apt-get update
   sudo apt-get install -y software-properties-common
   sudo add-apt-repository -y ppa:ondrej/php
   sudo apt-get update

   # Cài đặt PHP và các extension cần thiết
   sudo apt-get install -y php8.2 php8.2-cli php8.2-imagick

   # Kích hoạt FFI trong php.ini
   sudo sed -i 's/;ffi.enable.*/ffi.enable=true/' /etc/php/8.2/cli/php.ini
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   sudo dnf install php php-cli php-pecl-imagick

   # Kích hoạt FFI trong php.ini
   sudo sed -i 's/;ffi.enable.*/ffi.enable=true/' /etc/php.ini
   ```

   **macOS:**

   ```sh
   brew install php imagemagick
   pecl install imagick

   # Kích hoạt FFI trong php.ini
   echo "ffi.enable=true" >> $(php --ini | grep "Loaded Configuration" | sed -e "s|.*:\s*||")
   ```

   **Windows:**

   - Tải PHP từ https://windows.php.net/download/
   - Cài đặt ImageMagick và php_imagick.dll
   - Kích hoạt trong php.ini:
     ```ini
     ffi.enable=true
     extension=imagick
     ```

2. Xác minh cài đặt

   ```sh
   php --version
   php -m | grep -E "(FFI|imagick)"
   ```

### 3. Cách chạy

```sh
cd examples/PHP/anpr
php anpr.php
```

### 4. Tính năng

- **readImageFile**: Xử lý trực tiếp tệp hình ảnh bằng `anpr_read_file()`
- **readEncodedImage**: Xử lý dữ liệu hình ảnh được mã hóa (JPEG, PNG, v.v.) bằng `anpr_read_pixels()` với định dạng 'encoded'
- **readPixelBuffer**: Xử lý dữ liệu pixel thô bằng extension Imagick

### 5. Tham khảo API

#### Lớp TSANPR

```php
class TSANPR {
    public function __construct(string $libraryPath);
    public function anprInitialize(string $mode): string;
    public function anprReadFile(string $imgFileName, string $outputFormat, string $options): string;
    public function anprReadPixels(string $pixels, int $width, int $height, int $stride,
                                   string $pixelFormat, string $outputFormat, string $options): string;
}
```

#### Tùy chọn nhận dạng

| Tùy chọn | Mô tả |
|----------|-------|
| `""` | Nhận dạng biển số đơn (mặc định) |
| `"vm"` | Nhận dạng nhiều biển số gắn trên xe |
| `"vmb"` | Nhận dạng nhiều biển số (bao gồm xe máy) |
| `"vms"` | Nhận dạng với phát hiện xung quanh |
| `"dms"` | Phát hiện nhiều đối tượng xung quanh (xe) |
| `"dmsr"` | Phát hiện đối tượng và nhận dạng biển số |
| `"dmsri<tọa độ>"` | Nhận dạng trong Vùng quan tâm |

#### Định dạng đầu ra

`"text"`, `"json"`, `"yaml"`, `"xml"`, `"csv"`

### 6. Khắc phục sự cố

**FFI không được tải:**
```sh
# Kiểm tra trạng thái FFI
php -i | grep -i ffi

# Kích hoạt FFI trong php.ini
ffi.enable=true
```

**Imagick không khả dụng:**
```sh
# Ubuntu/Debian
sudo apt-get install php8.2-imagick

# Xác minh
php -m | grep imagick
```
