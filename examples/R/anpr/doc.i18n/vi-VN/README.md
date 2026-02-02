[English](../../) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ R

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/R/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/R/anpr)

### 1. Sao chép các tệp Engine

_**[Lưu ý]** Trong ví dụ này, tệp engine được giải nén vào thư mục examples/bin/ để chia sẻ với các ví dụ khác. Tuy nhiên, khi triển khai thực tế, tệp engine thường được sao chép vào thư mục chứa tệp thực thi của ứng dụng._

- Đối với Windows x86 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86_64`
  ```sh
  7z x tsanpr*-windows-x86_64.7z
  ```
- Đối với Windows x86 32-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86`
  ```sh
  7z x tsanpr*-windows-x86.7z
  ```
- Đối với Linux x86 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/linux-x86_64`
  ```sh
  tar xvf tsanpr-linux-x86_64.tar.xz
  ```
- Đối với Linux arm 64-bit
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
  └── R
      └── anpr                   # thư mục nguồn
         ├── anpr.R              # script ví dụ chính
         ├── tsanpr.R            # lớp wrapper TSANPR R6
         ├── src/                # mã nguồn C wrapper
         │   ├── tsanpr_r.c
         │   ├── Makevars
         │   └── Makevars.win
         └── DESCRIPTION
  ```

### 2. Yêu cầu trước

1. Cài đặt R và các phụ thuộc hệ thống

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get update
   # Cài đặt R và công cụ phát triển
   sudo apt-get install -y r-base r-base-dev
   # Cài đặt thư viện hệ thống cho các gói R
   sudo apt-get install -y libcurl4-openssl-dev libmagick++-dev
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   # CentOS/RHEL 8+ / Fedora
   sudo dnf install -y R R-devel
   sudo dnf install -y libcurl-devel ImageMagick-c++-devel

   # CentOS/RHEL 7
   sudo yum install -y R R-devel
   sudo yum install -y libcurl-devel ImageMagick-c++-devel
   ```

   **Windows:**

   - Tải R từ https://cran.r-project.org/bin/windows/base/
   - Cài đặt Rtools từ https://cran.r-project.org/bin/windows/Rtools/

2. Cài đặt các gói R cần thiết

   ```sh
   # Cài đặt gói R6 (bắt buộc)
   sudo Rscript -e 'install.packages("R6", repos="https://cloud.r-project.org")'

   # Cài đặt gói magick (tùy chọn, cho readPixelBuffer)
   sudo Rscript -e 'install.packages("magick", repos="https://cloud.r-project.org")'
   ```

   _Lưu ý: Trên Windows, chạy lệnh Rscript mà không có `sudo`._

3. Biên dịch C wrapper

   **Linux/macOS:**

   ```sh
   cd examples/R/anpr
   R CMD SHLIB src/tsanpr_r.c -o src/tsanpr_r.so
   ```

   **Windows:**

   ```sh
   cd examples/R/anpr
   R CMD SHLIB src/tsanpr_r.c -o src/tsanpr_r.dll
   ```

### 3. Cách chạy

```sh
cd examples/R/anpr

# Chạy ví dụ ANPR
Rscript anpr.R
```

**Từ R console:**

```r
setwd("examples/R/anpr")
source("anpr.R")
```

**Chế độ tương tác:**

```r
# Tải wrapper TSANPR
source("tsanpr.R")

# Khởi tạo TSANPR
engine_path <- "../../bin/linux-x86_64/libtsanpr.so"  # Điều chỉnh cho nền tảng của bạn
tsanpr <- TSANPR$new(engine_path)

# Khởi tạo engine
tsanpr$anpr_initialize("text;country=KR")

# Xử lý hình ảnh
result <- tsanpr$anpr_read_file("../../img/KR/licensePlate.jpg", "text", "")
print(result)
```

### 4. Tính năng

- **readImageFile**: Xử lý trực tiếp tệp hình ảnh sử dụng `anpr_read_file()`
- **readEncodedImage**: Xử lý byte hình ảnh đã mã hóa sử dụng `anpr_read_pixels()` với định dạng 'encoded'
- **readPixelBuffer**: Xử lý dữ liệu pixel RGB thô sử dụng gói magick
- **Tải động**: Thư viện TSANPR được tải tại thời điểm chạy thông qua C wrapper
- **Đa nền tảng**: Hỗ trợ Windows và Linux (x86_64, ARM64)

### 5. Tham khảo API

#### Lớp TSANPR

Lớp R6 `TSANPR` cung cấp các phương thức sau:

```r
TSANPR <- R6Class("TSANPR",
  public = list(
    initialize = function(library_path),
    anpr_initialize = function(mode),
    anpr_read_file = function(img_file_name, output_format, options),
    anpr_read_pixels = function(pixels, width, height, stride, pixel_format, output_format, options),
    is_loaded = function()
  )
)
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
| `"dmsri<coords>"` | Nhận dạng trong Vùng quan tâm |

#### Định dạng đầu ra

`"text"`, `"json"`, `"yaml"`, `"xml"`, `"csv"`

### 6. Ghi chú triển khai

Ví dụ này sử dụng C wrapper (`src/tsanpr_r.c`) để kết nối FFI của R với thư viện TSANPR:

**Tải thư viện động:**
- **Linux**: Sử dụng `dlopen()`, `dlsym()`, `dlclose()` từ libdl
- **Windows**: Sử dụng `LoadLibrary()`, `GetProcAddress()`, `FreeLibrary()` từ kernel32

**Tích hợp R:**
- Sử dụng giao diện `.Call()` của R để trao đổi dữ liệu hiệu quả
- Đăng ký đúng các routine C thông qua `R_registerRoutines()`
- Lớp R6 cung cấp giao diện hướng đối tượng rõ ràng

Mã R tải C wrapper đã biên dịch, C wrapper lần lượt tải động và gọi các hàm thư viện TSANPR.

### 7. Xử lý sự cố

**Vấn đề biên dịch:**

- Đảm bảo các gói phát triển R đã được cài đặt (`r-base-dev` trên Debian/Ubuntu)
- Trên Windows, cài đặt Rtools và thêm vào PATH
- Kiểm tra trình biên dịch C có sẵn: `R CMD config CC`

**Vấn đề tải thư viện:**

- Xác minh wrapper đã biên dịch tồn tại trong thư mục `src/`
- Đảm bảo đường dẫn thư viện TSANPR chính xác
- Trên Linux, đặt `LD_LIBRARY_PATH` nếu cần

**Phụ thuộc gói:**

- Cài đặt gói R6: `install.packages("R6")`
- Để xử lý pixel buffer: `install.packages("magick")`
