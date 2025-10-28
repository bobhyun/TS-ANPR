[English](../../README.md) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ D

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/D/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/D/anpr)

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
  └── D
      └── anpr                   # thư mục nguồn
         ├── anpr.d
         ├── dub.json
         └── tsanpr.d
  ```

### 2. Yêu cầu tiên quyết

1. Cài đặt trình biên dịch D (DMD, LDC, hoặc GDC)

   **Windows:**

   ```sh
   # Tải xuống và cài đặt DMD từ https://dlang.org/download.html
   # Hoặc sử dụng chocolatey
   choco install dmd
   ```

   **Ubuntu / Debian:**

   ```sh
   # Cài đặt bằng snap
   sudo snap install dmd --classic
   sudo snap install dub --classic

   # Hoặc cài đặt bằng apt
   sudo wget https://netcologne.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list
   sudo apt-get update && sudo apt-get -y --allow-unauthenticated install --reinstall d-apt-keyring
   sudo apt-get update && sudo apt-get install dmd-compiler dub
   ```

   **Oracle Linux / RedHat (RHEL) / CentOS:**

   ```sh
   # Cài đặt bằng snap
   sudo snap install dmd --classic
   sudo snap install dub --classic

   # Hoặc tải xuống và cài đặt thủ công từ https://dlang.org/download.html
   curl -fsS https://dlang.org/install.sh | bash -s dmd
   source ~/dlang/dmd-*/activate
   ```

2. Xác minh cài đặt

   ```sh
   dmd --version
   ```

### 3. Cách chạy

1. Điều hướng đến thư mục ví dụ D

   ```sh
   cd D/anpr
   ```

2. Biên dịch và chạy ví dụ

   **Sử dụng DUB (khuyến nghị):**

   ```sh
   # Build và chạy
   dub run

   # Chỉ build
   dub build

   # Build phiên bản release
   dub build --build=release

   # Chạy
   ./bin/anpr
   ```

### 4. Ghi chú

- Triển khai D này cung cấp cùng chức năng như các ví dụ ngôn ngữ khác
- Sử dụng khả năng tương tác C xuất sắc của D để giao tiếp với thư viện TSANPR gốc
- Khả năng lập trình hệ thống của D và các tính năng ngôn ngữ hiện đại cung cấp cả hiệu suất và an toàn
- Hỗ trợ đa nền tảng cho Windows và Linux
- Quản lý bộ nhớ tích hợp và thu gom rác của D đơn giản hóa việc phát triển

### 5. Tính năng

- **Nhận dạng dựa trên tệp**: Xử lý trực tiếp các tệp hình ảnh
- **Xử lý hình ảnh được mã hóa**: Xử lý dữ liệu hình ảnh được mã hóa (JPEG, PNG, v.v.)
- **Xử lý bộ đệm pixel**: Xử lý dữ liệu pixel thô (sử dụng thư viện imageformats)
- **Nhiều định dạng đầu ra**: Hỗ trợ đầu ra văn bản, JSON, YAML, XML và CSV
- **Nhiều chế độ nhận dạng**: Biển số đơn, nhiều biển số, phát hiện xe, v.v.
- **Vùng quan tâm (RoI)**: Xử lý các khu vực cụ thể trong hình ảnh
- **Hỗ trợ đa quốc gia**: Hỗ trợ các định dạng biển số khác nhau (KR, JP, VN, v.v.)

### 6. Tham khảo API

#### Lớp TSAnpr

**Constructor:**

- `this(string libraryPath)`: Khởi tạo với đường dẫn thư viện gốc

**Phương thức cốt lõi:**

- `string anprInitialize(string enginePath)`: Khởi tạo engine ANPR
- `string anprReadFile(string imagePath, string options, string outputFormat)`: Xử lý tệp hình ảnh
- `string anprReadPixels(ubyte[] pixels, int width, int height, int channels, string options, string outputFormat)`: Xử lý dữ liệu pixel

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

**Vấn đề biên dịch:**

- Đảm bảo trình biên dịch D được cài đặt đúng cách
- Sử dụng cờ trình biên dịch phù hợp cho nền tảng đích của bạn
- Xem xét sử dụng trình quản lý gói DUB cho các dự án phức tạp

**Vấn đề cụ thể theo nền tảng:**

- **Windows**: Đảm bảo Visual C++ Redistributable đã được cài đặt
- **Linux**: Cài đặt các thư viện hệ thống cần thiết và đảm bảo quyền thư viện là chính xác
