[English](../../README.md) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ Gleam

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Gleam/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Gleam/anpr)

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
  │   │   ├── tsanpr-*.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86           # thư mục engine cho Windows (x86)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-*.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64           # thư mục engine cho Linux (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-*.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # thư mục engine cho Linux (arm64)
  │       ├── libtsanpr.so
  │       ├── tsanpr-*.eon
  │       └── tshelper
  ├── img                        # thư mục hình ảnh
  └── Gleam
      └── anpr                   # thư mục dự án
         ├── c_src               # Tệp nguồn C NIF
         │   ├── tsanpr_nif.c    # Triển khai NIF
         │   └── stb_image.h     # Thư viện giải mã hình ảnh
         ├── src                 # Tệp nguồn Gleam/Erlang
         │   ├── anpr.gleam      # Module chính
         │   ├── tsanpr.gleam    # Module API TSANPR
         │   ├── tsanpr_ffi.erl  # Module wrapper NIF
         │   └── anpr_ffi.erl    # Hàm trợ giúp
         ├── priv                # Thư viện NIF đã biên dịch (được tạo)
         │   └── tsanpr_nif.dll/.so
         ├── gleam.toml          # Cấu hình dự án Gleam
         ├── manifest.toml       # Manifest phụ thuộc Gleam
         ├── build_nif.bat       # Script build NIF cho Windows
         ├── build_nif.sh        # Script build NIF cho Linux
         ├── Makefile            # Makefile Linux
         ├── Makefile.win        # Makefile Windows
         └── build               # Đầu ra build Gleam (được tạo)
            └── dev/erlang/anpr
               ├── ebin          # Tệp .beam đã biên dịch
               └── priv          # Thư viện NIF cho runtime
  ```

### 2. Yêu cầu tiên quyết

1. Cài đặt Gleam và Erlang

   **Windows:**

   ```sh
   # Cài đặt Erlang trước
   # Tải xuống từ https://www.erlang.org/downloads

   # Cài đặt Gleam
   # Tải xuống từ https://gleam.run/getting-started/installing/
   ```

   **Linux:**

   ```sh
   # Cài đặt Erlang
   sudo apt-get install erlang

   # Cài đặt Gleam (x86_64)
   cd /tmp
   wget https://github.com/gleam-lang/gleam/releases/download/v1.14.0/gleam-v1.14.0-x86_64-unknown-linux-musl.tar.gz
   tar xzf gleam-v1.14.0-x86_64-unknown-linux-musl.tar.gz
   sudo mv gleam /usr/local/bin/

   # Cho ARM64 (aarch64)
   wget https://github.com/gleam-lang/gleam/releases/download/v1.14.0/gleam-v1.14.0-aarch64-unknown-linux-musl.tar.gz
   tar xzf gleam-v1.14.0-aarch64-unknown-linux-musl.tar.gz
   sudo mv gleam /usr/local/bin/
   ```

2. Xác minh cài đặt

   ```sh
   gleam --version
   erl -version
   ```

### 3. Cách chạy

1. Điều hướng đến thư mục ví dụ Gleam

   ```sh
   cd Gleam/anpr
   ```

2. Chạy ví dụ

   ```sh
   # Cài đặt dependencies
   gleam deps download

   # Chạy ví dụ
   gleam run

   # Hoặc build và chạy
   gleam build
   gleam run
   ```

### 4. Ghi chú

- Triển khai Gleam này cung cấp cùng chức năng như các ví dụ ngôn ngữ khác
- Sử dụng khả năng tương tác Erlang của Gleam để giao tiếp với thư viện TSANPR gốc
- Tính an toàn kiểu và paradigm lập trình hàm của Gleam cung cấp độ tin cậy
- Chạy trên Erlang Virtual Machine (BEAM) để có khả năng đồng thời xuất sắc
- Lưu ý: Ví dụ này sử dụng tích hợp thư viện gốc đơn giản hóa cho mục đích demo

### 5. Tính năng

- **Nhận dạng dựa trên tệp**: Xử lý trực tiếp các tệp hình ảnh
- **Xử lý hình ảnh được mã hóa**: Xử lý dữ liệu hình ảnh được mã hóa (JPEG, PNG, v.v.)
- **Xử lý bộ đệm pixel**: Xử lý dữ liệu pixel thô (triển khai đơn giản hóa)
- **Nhiều định dạng đầu ra**: Hỗ trợ đầu ra văn bản, JSON, YAML, XML và CSV
- **Nhiều chế độ nhận dạng**: Biển số đơn, nhiều biển số, phát hiện xe, v.v.
- **Vùng quan tâm (RoI)**: Xử lý các khu vực cụ thể trong hình ảnh
- **Hỗ trợ đa quốc gia**: Hỗ trợ các định dạng biển số khác nhau (KR, JP, VN, v.v.)

### 6. Tham khảo API

#### Module TSANPR

Triển khai Gleam cung cấp các hàm sau:

**Khởi tạo:**

- `tsanpr.new(library_path: String) -> Result(TSANPR, String)`: Tạo instance TSANPR

**Hàm cốt lõi:**

- `tsanpr.initialize(tsanpr: TSANPR, mode: String) -> Result(String, String)`: Khởi tạo engine ANPR
- `tsanpr.read_file(tsanpr: TSANPR, img_file_name: String, output_format: String, options: String) -> Result(String, String)`: Xử lý tệp hình ảnh
- `tsanpr.read_pixels(tsanpr: TSANPR, pixels: List(Int), width: Int, height: Int, stride: Int, pixel_format: String, output_format: String, options: String) -> Result(String, String)`: Xử lý dữ liệu pixel

#### Tùy chọn nhận dạng

- `""`: Nhận dạng biển số đơn (mặc định)
- `"vm"`: Nhận dạng nhiều biển số gắn trên xe
- `"vmb"`: Nhận dạng nhiều biển số gắn trên xe (bao gồm xe máy)
- `"vms"`: Nhận dạng nhiều biển số gắn trên xe với phát hiện xung quanh
- `"dms"`: Nhận dạng nhiều đối tượng xung quanh (xe)
- `"dmsr"`: Nhận dạng nhiều đối tượng xung quanh (xe) và biển số
- `"dmsri<tọa độ>"`: Nhận dạng trong Vùng quan tâm

#### Định dạng đầu ra

- `"text"`: Đầu ra văn bản thuần túy
- `"json"`: Đầu ra định dạng JSON
- `"yaml"`: Đầu ra định dạng YAML
- `"xml"`: Đầu ra định dạng XML
- `"csv"`: Đầu ra định dạng CSV

### 7. Khắc phục sự cố

**Vấn đề tải thư viện:**

- Đảm bảo đường dẫn thư viện TSANPR là chính xác
- Kiểm tra tất cả các phụ thuộc hệ thống đã được cài đặt
- Xác minh quyền thư viện là chính xác

**Vấn đề build:**

- Đảm bảo Gleam và Erlang được cài đặt đúng cách
- Chạy `gleam deps download` để cài đặt dependencies
- Sử dụng `gleam check` để xác minh cấu trúc dự án

**Vấn đề cụ thể theo nền tảng:**

- **Windows**: Đảm bảo Visual C++ Redistributable đã được cài đặt
- **Linux**: Cài đặt các thư viện hệ thống cần thiết và đảm bảo quyền thư viện là chính xác
- **Erlang VM**: Đảm bảo Erlang/OTP được cài đặt và cấu hình đúng cách
