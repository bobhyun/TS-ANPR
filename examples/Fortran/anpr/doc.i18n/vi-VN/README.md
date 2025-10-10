[English](../../README.md) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ Fortran

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Fortran/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Fortran/anpr)

### 1. Sao chép các tệp Engine

_**[Lưu ý]** Trong ví dụ này, tệp engine được giải nén vào thư mục examples/bin/ để chia sẻ với các ví dụ khác. Tuy nhiên, đối với triển khai thực tế, tệp engine thường được sao chép vào thư mục nơi tệp thực thi của ứng dụng được đặt._

- Cho Windows x86 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86_64`
  ```sh
  tar xvf tsanpr*-windows-x86_64.tar.xz
  ```
- Cho Windows x86 32-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86`
  ```sh
  tar xvf tsanpr*-windows-x86.tar.xz
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
  └── Fortran
      └── anpr                   # thư mục dự án
         ├── src                 # thư mục nguồn
         │   ├── anpr.f90
         │   └── tsanpr_module.f90
         ├── build               # thư mục đầu ra build (được tạo bởi make)
         └── Makefile
  ```

### 2. Yêu cầu tiên quyết

1. Cài đặt trình biên dịch Fortran và công cụ build

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get update
   sudo apt-get install gfortran build-essential
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   # CentOS/RHEL 7
   sudo yum install gcc-gfortran make

   # CentOS/RHEL 8+ / Fedora
   sudo dnf install gcc-gfortran make
   ```

   **Windows (MinGW/MSYS2):**

   ```sh
   # Cài đặt MSYS2 trước, sau đó:
   pacman -S mingw-w64-x86_64-gcc-fortran mingw-w64-x86_64-make
   ```

2. Xác minh cài đặt

   ```sh
   gfortran --version
   make --version
   ```

### 3. Cách build

1. Điều hướng đến thư mục ví dụ Fortran

   ```sh
   cd Fortran/anpr
   ```

2. Build ví dụ

   ```sh
   make all
   ```

3. Dọn dẹp các tệp build (nếu cần)

   ```sh
   make clean
   ```

### 4. Cách chạy

1. Chạy ví dụ `anpr`

   ```sh
   ./build/anpr
   ```

   Hoặc trên Windows:

   ```sh
   build/anpr.exe
   ```

### 5. Ghi chú

- Triển khai Fortran này cung cấp cùng chức năng như các ví dụ ngôn ngữ khác
- Sử dụng tiêu chuẩn Fortran 2008 hiện đại với ISO C binding để tương tác
- Tải thư viện động được xử lý thông qua API cụ thể của hệ thống (dlopen trên Unix, LoadLibrary trên Windows)

- Xử lý bộ đệm pixel được đơn giản hóa trong ví dụ này - triển khai đầy đủ sẽ yêu cầu tích hợp với thư viện xử lý hình ảnh
- Hỗ trợ biên dịch đa nền tảng thông qua các chỉ thị tiền xử lý

### 6. Tính năng

- **Nhận dạng dựa trên tệp**: Xử lý trực tiếp các tệp hình ảnh
- **Xử lý hình ảnh được mã hóa**: Xử lý dữ liệu hình ảnh được mã hóa (JPEG, PNG, v.v.)
- **Xử lý bộ đệm pixel**: Xử lý dữ liệu pixel thô (triển khai đơn giản hóa)
- **Nhiều định dạng đầu ra**: Hỗ trợ đầu ra văn bản, JSON, YAML, XML và CSV
- **Nhiều chế độ nhận dạng**: Biển số đơn, nhiều biển số, phát hiện xe, v.v.
- **Vùng quan tâm (RoI)**: Xử lý các khu vực cụ thể trong hình ảnh
- **Hỗ trợ đa quốc gia**: Hỗ trợ các định dạng biển số khác nhau (KR, JP, VN, v.v.)
- **Tương thích đa nền tảng**: Hỗ trợ Windows và Linux

### 7. Tham khảo API

#### Module TSANPR

`tsanpr_module` cung cấp các kiểu và thủ tục sau:

**Kiểu:**

- `tsanpr_handle`: Handle cho instance thư viện TSANPR

**Khởi tạo:**

- `tsanpr_init(tsanpr, library_path, status)`: Khởi tạo TSANPR với đường dẫn thư viện
- `tsanpr_cleanup(tsanpr)`: Dọn dẹp tài nguyên TSANPR

**Hàm cốt lõi:**

- `tsanpr_initialize(tsanpr, mode, error_msg, status)`: Khởi tạo engine ANPR
- `tsanpr_read_file(tsanpr, img_file_name, output_format, options, result, status)`: Xử lý tệp hình ảnh
- `tsanpr_read_pixels(tsanpr, pixels, width, height, stride, pixel_format, output_format, options, result, status)`: Xử lý dữ liệu pixel

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
