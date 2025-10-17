[English](../../) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ Zig

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Zig/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Zig/anpr)

### 1. Sao chép các tệp Engine

_**[Lưu ý]** Trong ví dụ này, chúng tôi đặt các tệp engine trong thư mục examples/bin/ để chia sẻ giữa các ví dụ khác. Khi triển khai thực tế, bạn thường nên sao chép các tệp engine vào cùng thư mục với tệp thực thi của ứng dụng._

- Windows x86 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86_64`
  ```sh
  7z x tsanpr*-windows-x86_64.7z
  ```
- Windows x86 32-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86`
  ```sh
  7z x tsanpr*-windows-x86.7z
  ```
- Linux x86 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/linux-x86_64`
  ```sh
  tar xvf tsanpr-linux-x86_64.tar.xz
  ```
- Linux arm 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/linux-aarch64`
  ```sh
  tar xvf tsanpr-linux-aarch64.tar.xz
  ```
- Cấu trúc thư mục
  ```sh
  examples
  ├── bin
  │   ├─── windows-x86_64        # Thư mục engine cho Windows (x86_64)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86           # Thư mục engine cho Windows (x86)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64           # Thư mục engine cho Linux (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # Thư mục engine cho Linux (arm64)
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # Thư mục hình ảnh
  └── Zig
      └── anpr                   # Thư mục mã nguồn
         ├── build.zig
         ├── src/
         │   ├── main.zig
         │   ├── tsanpr.c
         │   ├── tsanpr.h
         │   └── tsanpr.zig
         └── zig-out/
             └── bin/            # Thư mục đầu ra
  ```

### 2. Yêu cầu

1. Cài đặt Zig (khuyến nghị phiên bản ổn định mới nhất)

   **Windows:**

   ```sh
   # Tải xuống từ https://ziglang.org/download/
   # Hoặc sử dụng Scoop
   scoop install zig
   ```

   **Linux:**

   ```sh
   # Tải xuống từ https://ziglang.org/download/
   # Hoặc sử dụng trình quản lý gói (tùy theo bản phân phối)
   sudo snap install zig --classic --beta
   ```

2. Xác minh cài đặt

   ```sh
   zig version
   ```

### 3. Cách chạy

1. Di chuyển đến thư mục ví dụ Zig

   ```sh
   cd Zig/anpr
   ```

2. Xây dựng và chạy ví dụ

   ```sh
   # Xây dựng và chạy
   zig build run

   # Chỉ xây dựng
   zig build

   # Xây dựng với tối ưu hóa
   zig build -Doptimize=ReleaseFast

   # Chạy kiểm thử
   zig build test
   ```

### 4. Ghi chú

- Triển khai Zig cung cấp chức năng tương tự như các ví dụ ngôn ngữ khác
- Sử dụng khả năng tương tác C của Zig để giao tiếp với thư viện TSANPR gốc
- Tính an toàn tại thời điểm biên dịch và hiệu suất của Zig làm cho nó xuất sắc cho lập trình hệ thống
- Hỗ trợ đa nền tảng cho Windows và Linux
- Tải thư viện trong thời gian chạy để có tính linh hoạt triển khai tốt hơn

### 5. Tính năng

- **Nhận dạng dựa trên tệp**: Xử lý tệp hình ảnh trực tiếp
- **Xử lý hình ảnh được mã hóa**: Xử lý dữ liệu hình ảnh được mã hóa (JPEG, PNG, v.v.)
- **Xử lý bộ đệm pixel**: Xử lý dữ liệu pixel thô (triển khai đơn giản hóa)
- **Nhiều định dạng đầu ra**: Hỗ trợ đầu ra văn bản, JSON, YAML, XML và CSV
- **Nhiều chế độ nhận dạng**: Biển số đơn, nhiều biển số, phát hiện xe, v.v.
- **Vùng quan tâm (RoI)**: Xử lý các khu vực cụ thể trong hình ảnh
- **Hỗ trợ đa quốc gia**: Hỗ trợ các định dạng biển số khác nhau (KR, JP, VN, v.v.)

### 6. Tham chiếu API

#### Hàm TSANPR

Triển khai Zig cung cấp các hàm sau thông qua tương tác C:

**Khởi tạo:**

- `TSANPR_load(library_path)`: Tải thư viện TSANPR
- `anpr_initialize(mode)`: Khởi tạo engine ANPR

**Hàm cốt lõi:**

- `anpr_read_file(img_file_name, output_format, options)`: Xử lý tệp hình ảnh
- `anpr_read_pixels(pixels, width, height, stride, pixel_format, output_format, options)`: Xử lý dữ liệu pixel

#### Tùy chọn nhận dạng

- `""`: Nhận dạng biển số đơn (mặc định)
- `"vm"`: Nhận dạng nhiều biển số gắn trên xe
- `"vmb"`: Nhận dạng nhiều biển số gắn trên xe (bao gồm xe máy)
- `"vms"`: Nhận dạng nhiều biển số xe với phát hiện xung quanh
- `"dms"`: Nhận dạng nhiều đối tượng xung quanh (xe)
- `"dmsr"`: Nhận dạng nhiều đối tượng xung quanh (xe) và biển số
- `"dmsri<tọa độ>"`: Nhận dạng trong vùng quan tâm

#### Định dạng đầu ra

- `"text"`: Đầu ra văn bản thuần
- `"json"`: Đầu ra định dạng JSON
- `"yaml"`: Đầu ra định dạng YAML
- `"xml"`: Đầu ra định dạng XML
- `"csv"`: Đầu ra định dạng CSV
