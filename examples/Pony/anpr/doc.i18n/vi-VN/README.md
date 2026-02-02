[English](../../) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ Pony

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Pony/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Pony/anpr)

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
  └── Pony
      └── anpr                   # thư mục dự án
         ├── src/                # gói chính
         │   ├── main.pony
         │   └── tsanpr.pony
         ├── ffi/                # C FFI wrapper
         │   ├── tsanpr_wrapper.c
         │   ├── tsanpr_wrapper.h
         │   ├── image_loader.c
         │   └── stb_image.h
         └── Makefile
  ```

### 2. Yêu cầu trước

1. Cài đặt trình biên dịch Pony

   **Ubuntu/Debian:**

   ```sh
   # Sử dụng ponyup (khuyến nghị)
   curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/ponylang/ponyup/latest-release/ponyup-init.sh | sh
   source ~/.profile
   ponyup update ponyc release
   ```

   **macOS:**

   ```sh
   brew install ponyc
   ```

   **Windows:**

   ```sh
   # Sử dụng Chocolatey
   choco install ponyc

   # Hoặc tải từ https://github.com/ponylang/ponyc/releases
   ```

2. Cài đặt trình biên dịch C (GCC hoặc Clang)

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get install build-essential
   ```

   **macOS:**

   ```sh
   xcode-select --install
   ```

   **Windows:**

   - Cài đặt Visual Studio Build Tools hoặc MinGW-w64

3. Xác minh cài đặt

   ```sh
   ponyc --version
   gcc --version
   ```

### 3. Cách chạy

```sh
cd examples/Pony/anpr

# Build và chạy
make run

# Chỉ build
make

# Build phiên bản release
make release

# Dọn dẹp tệp build
make clean
```

### 4. Tính năng

- **readImageFile**: Xử lý trực tiếp tệp hình ảnh sử dụng `anpr_read_file()`
- **readEncodedImage**: Xử lý byte hình ảnh đã mã hóa sử dụng `anpr_read_pixels()` với định dạng 'encoded'
- **readPixelBuffer**: Xử lý dữ liệu pixel RGB thô sử dụng thư viện stb_image
- **Tải động**: Thư viện TSANPR được tải tại thời điểm chạy sử dụng dlopen/LoadLibrary
- **Đa nền tảng**: Hỗ trợ Windows và Linux (x86_64, ARM64)

### 5. Tham khảo API

#### Lớp TSANPR

```pony
class TSANPR
  new create(library_path: String) ?
  fun ref initialize(mode: String): String
  fun ref read_file(img_file_name: String, output_format: String, options: String): String
  fun ref read_pixels(pixels: Array[U8] val, width: U64, height: U64, stride: I64,
                     pixel_format: String, output_format: String, options: String): String
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

Ví dụ này sử dụng wrapper C để xử lý tải thư viện động và xử lý hình ảnh:

**Tải thư viện động (`tsanpr_wrapper.c`):**
- **Linux**: Sử dụng `dlopen()`, `dlsym()`, `dlclose()` từ libdl
- **Windows**: Sử dụng `LoadLibrary()`, `GetProcAddress()`, `FreeLibrary()` từ kernel32

**Xử lý hình ảnh (`image_loader.c`):**
- Sử dụng thư viện single-header [stb_image](https://github.com/nothings/stb) để giải mã hình ảnh
- Cung cấp hàm tiện ích `image_anpr_read()` để tải hình ảnh và gọi ANPR trực tiếp

Mã Pony gọi các hàm wrapper C thông qua FFI, các hàm này lần lượt gọi các hàm thư viện TSANPR được tải động.
