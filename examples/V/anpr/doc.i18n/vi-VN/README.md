[English](../../) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ V

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/V/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/V/anpr)

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
  └── V/
      └── anpr/                 # thư mục module V
          ├── v.mod             # định nghĩa module
          ├── anpr.v            # ví dụ chính
          └── tsanpr.v          # module wrapper TSANPR
  ```

### 2. Yêu cầu tiên quyết

1. Cài đặt V (khuyến nghị phiên bản mới nhất)

   **Windows:**

   ```sh
   # Sử dụng Scoop
   scoop install vlang

   # Hoặc tải binary đã biên dịch:
   # https://github.com/vlang/v/releases
   ```

   **Linux:**

   ```sh
   # Clone và build từ source
   git clone https://github.com/vlang/v
   cd v
   make
   sudo ./v symlink
   ```

2. Xác minh cài đặt

   ```sh
   v version
   ```

### 3. Cách chạy

```sh
cd examples/V/anpr

# Chạy ví dụ ANPR
v run .
```

**Các phương pháp khác:**

```sh
# Biên dịch và chạy riêng biệt
v .
./anpr        # Linux
anpr.exe      # Windows

# Biên dịch với tối ưu hóa
v -prod .
```

### 4. Tính năng

- **readImageFile**: Xử lý trực tiếp tệp hình ảnh sử dụng `anpr_read_file()`
- **readEncodedImage**: Xử lý byte hình ảnh đã mã hóa sử dụng `anpr_read_pixels()` với định dạng 'encoded'
- **readPixelBuffer**: Xử lý dữ liệu pixel RGB thô sử dụng module `stbi` của V (stb_image)
- **Tải động**: Thư viện TSANPR được tải tại thời điểm chạy thông qua module `dl` của V
- **Đa nền tảng**: Hỗ trợ Windows và Linux (x86_64, x86, ARM64)

### 5. Tham khảo API

#### Module TSANPR

Module `tsanpr` cung cấp:

```v
// Tạo instance TSANPR
pub fn new(library_path string) !TSANPR

// Các phương thức TSANPR
pub fn (mut t TSANPR) destroy()
pub fn (t &TSANPR) initialize(mode string) string
pub fn (t &TSANPR) read_file(img_file_name string, output_format string, options string) string
pub fn (t &TSANPR) read_pixels(pixels []u8, width u64, height u64, stride i64, pixel_format string, output_format string, options string) string
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

Ví dụ này sử dụng module `dl` của V để tải thư viện động:

**Tải thư viện động:**
- **Linux**: Sử dụng `dlopen()`, `dlsym()`, `dlclose()` thông qua module `dl` của V
- **Windows**: Sử dụng `LoadLibrary()`, `GetProcAddress()`, `FreeLibrary()` thông qua module `dl` của V

**Tích hợp V:**
- Khả năng tương tác C của V cho phép gọi con trỏ hàm trực tiếp đến thư viện gốc
- Chuyển đổi chuỗi tự động giữa chuỗi V và chuỗi C
- An toàn bộ nhớ với `defer` để dọn dẹp

**Xử lý bộ đệm pixel:**
Hàm `readPixelBuffer` sử dụng module `stbi` tích hợp của V (wrapper stb_image) để giải mã hình ảnh. Nó tải hình ảnh, trích xuất dữ liệu pixel RGB thô và truyền cho `anpr_read_pixels()` với định dạng pixel thích hợp.

### 7. Khắc phục sự cố

**Vấn đề biên dịch:**

- Đảm bảo V được cài đặt đúng cách và có trong PATH
- Kiểm tra phiên bản V: `v version`
- Cập nhật V nếu cần: `v up`

**Vấn đề tải thư viện:**

- Xác minh thư viện TSANPR tồn tại ở vị trí mong đợi
- Trên Linux, kiểm tra `LD_LIBRARY_PATH` nếu cần
- Đảm bảo kiến trúc thư viện khớp với bản dựng V (64-bit vs 32-bit)

**Vấn đề thời gian chạy:**

- Kiểm tra các tệp engine (`.eon`) nằm trong cùng thư mục với thư viện
- Xác minh giấy phép đã được cài đặt bằng `tshelper`
