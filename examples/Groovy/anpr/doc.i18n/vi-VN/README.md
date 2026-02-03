[English](../../README.md) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ Groovy

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Groovy/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Groovy/anpr)

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
  ```
  examples/
  ├── bin/
  │   ├── windows-x86_64/       # engine cho Windows (x86_64)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── windows-x86/          # engine cho Windows (x86)
  │   │   └── ...
  │   ├── linux-x86_64/         # engine cho Linux (x86_64)
  │   │   └── ...
  │   └── linux-aarch64/        # engine cho Linux (arm64)
  │       └── ...
  ├── img/
  └── Groovy/
      └── anpr/                 # thư mục mã nguồn Groovy
          ├── anpr.groovy       # ví dụ chính
          └── TSANPR.groovy     # module wrapper JNA TSANPR
  ```

### 2. Yêu cầu tiên quyết

1. Cài đặt Java JDK 8 trở lên

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

   - Tải và cài đặt JDK từ https://adoptium.net/ hoặc https://www.oracle.com/java/technologies/downloads/

2. Cài đặt Groovy

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

   - Tải từ https://groovy.apache.org/download.html
   - Hoặc sử dụng SDKMAN: `sdk install groovy`

3. Xác minh cài đặt

   ```sh
   java -version
   groovy --version
   ```

### 3. Cách chạy

```sh
cd examples/Groovy/anpr

# Chạy ví dụ ANPR
groovy anpr.groovy
```

### 4. Tính năng

- **readImageFile**: Xử lý trực tiếp tệp hình ảnh sử dụng `anpr_read_file()`
- **readEncodedImage**: Xử lý byte hình ảnh đã mã hóa sử dụng `anpr_read_pixels()` với định dạng 'encoded'
- **readPixelBuffer**: Xử lý dữ liệu pixel RGB thô sử dụng Java AWT BufferedImage
- **Tải động**: Thư viện TSANPR được tải tại thời điểm chạy thông qua JNA
- **Đa nền tảng**: Hỗ trợ Windows và Linux (x86_64, x86, ARM64)
- **Phụ thuộc tự động**: JNA được tự động tải xuống qua Grape (chú thích @Grab)

### 5. Tham khảo API

#### Module TSANPR

Lớp `TSANPR` cung cấp các phương thức sau:

```groovy
// Tải thư viện TSANPR
def tsanpr = new TSANPR(libraryPath)

// Khởi tạo engine ANPR
String error = tsanpr.initialize(mode)

// Đọc và xử lý tệp hình ảnh
String result = tsanpr.readFile(imgFileName, outputFormat, options)

// Xử lý dữ liệu pixel trực tiếp
String result = tsanpr.readPixels(pixels, width, height, stride, pixelFormat, outputFormat, options)
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

### 6. Ghi chú triển khai

Ví dụ này sử dụng JNA (Java Native Access) để tải thư viện động:

**Tải thư viện động:**
- JNA cung cấp quyền truy cập dễ dàng vào thư viện chia sẻ gốc mà không cần JNI boilerplate
- Interface `TSANPRLibrary` định nghĩa chữ ký hàm gốc
- Thư viện được tải bằng phương thức `Native.load()`

**Tính năng Groovy:**
- Sử dụng Grape (chú thích @Grab) cho quản lý phụ thuộc tự động
- Thư viện JNA được tự động tải xuống khi chạy lần đầu
- Closures và tham chiếu phương thức cho việc chọn hàm linh hoạt

**Xử lý bộ đệm pixel:**
Hàm `readPixelBuffer` sử dụng `BufferedImage` của Java để tải và giải mã hình ảnh. Nó trích xuất dữ liệu pixel thô và truyền cho `anpr_read_pixels()` với định dạng pixel thích hợp sử dụng lớp `Memory` của JNA.

### 7. Khắc phục sự cố

**Không tìm thấy Groovy:**
```sh
# Kiểm tra cài đặt Groovy
groovy --version

# Ubuntu/Debian: Cài đặt Groovy
sudo apt-get install groovy

# Hoặc sử dụng SDKMAN
curl -s "https://get.sdkman.io" | bash
sdk install groovy
```

**Vấn đề phụ thuộc JNA:**
```sh
# Grape sẽ tự động tải JNA
# Nếu vấn đề vẫn tiếp tục, kiểm tra kết nối mạng hoặc cài đặt proxy

# Xóa cache Grape và thử lại
rm -rf ~/.groovy/grapes/net.java.dev.jna
groovy anpr.groovy
```

**Vấn đề tải thư viện:**
- Xác minh thư viện TSANPR tồn tại ở vị trí mong đợi
- Trên Linux, kiểm tra `LD_LIBRARY_PATH` nếu cần
- Đảm bảo kiến trúc thư viện khớp với JVM (64-bit vs 32-bit)

**Vấn đề thời gian chạy:**
- Kiểm tra các tệp engine (`.eon`) nằm trong cùng thư mục với thư viện
- Xác minh giấy phép đã được cài đặt bằng `tshelper`
