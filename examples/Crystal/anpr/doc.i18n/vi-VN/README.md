[English](../../README.md) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ Crystal

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Crystal/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Crystal/anpr)

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
  └── Crystal
      └── anpr                   # thư mục nguồn
         ├── anpr.cr
         └── tsanpr.cr
  ```

### 2. Yêu cầu tiên quyết

1. Cài đặt Crystal (khuyến nghị phiên bản 1.0.0 trở lên)

   **Windows:**

   ```sh
   # Sử dụng Scoop
   scoop install crystal

   # Hoặc tải xuống từ https://crystal-lang.org/install/on_windows/
   ```

   **Linux:**

   ```sh
   # Ubuntu/Debian
   curl -fsSL https://crystal-lang.org/install.sh | sudo bash

   # Hoặc sử dụng trình quản lý gói
   sudo apt-get install crystal
   ```

2. Xác minh cài đặt

   ```sh
   crystal --version
   ```

### 3. Cách chạy

1. Điều hướng đến thư mục ví dụ Crystal

   ```sh
   cd Crystal/anpr
   ```

2. Cài đặt các phụ thuộc

   ```sh
   # Cài đặt các shard cần thiết (StumpyPNG và StumpyJPEG để giải mã hình ảnh)
   shards install
   ```

   **Lưu ý Windows:** Shards yêu cầu symbolic links. Nếu gặp lỗi symbolic link, bạn có hai tùy chọn:

   - **Tùy chọn 1 (Khuyến nghị):** Bật Developer Mode trong Windows Settings
     - Vào Settings → Privacy & Security → For developers → Developer Mode → ON
     - Xem: https://learn.microsoft.com/vi-vn/windows/apps/get-started/enable-your-device-for-development

   - **Tùy chọn 2:** Chạy PowerShell hoặc Command Prompt với quyền Administrator
     ```powershell
     # Chạy với quyền Administrator
     shards install
     ```

3. Chạy ví dụ

   **Sử dụng Shards (khuyến nghị):**

   ```sh
   # Build và chạy
   shards build
   ./bin/anpr

   # Build với tối ưu hóa
   shards build --release
   ```

   **Biên dịch trực tiếp:**

   ```sh
   # Chạy trực tiếp
   crystal run anpr.cr

   # Hoặc biên dịch và chạy
   crystal build anpr.cr
   ./anpr

   # Biên dịch với tối ưu hóa
   crystal build --release anpr.cr
   ```

### 4. Ghi chú

- Triển khai Crystal này cung cấp cùng chức năng như các ví dụ ngôn ngữ khác
- Sử dụng binding `lib` của Crystal để giao tiếp với thư viện TSANPR gốc
- Cú pháp giống Ruby của Crystal với hiệu suất giống C làm cho nó lý tưởng cho lập trình hệ thống
- Hỗ trợ đa nền tảng cho Windows và Linux
- An toàn bộ nhớ với thu gom rác và kiểm tra null tại thời điểm biên dịch

### 5. Tính năng

- **Nhận dạng dựa trên tệp**: Xử lý trực tiếp các tệp hình ảnh bằng `anpr_read_file`
- **Xử lý hình ảnh được mã hóa**: Xử lý dữ liệu hình ảnh được mã hóa dưới dạng mảng byte (JPEG, PNG, v.v.)
- **Xử lý bộ đệm pixel**: Xử lý dữ liệu pixel được giải mã thành định dạng RGB bằng thư viện StumpyPNG và StumpyJPEG
- **Nhiều định dạng đầu ra**: Hỗ trợ đầu ra văn bản, JSON, YAML, XML và CSV
- **Nhiều chế độ nhận dạng**: Biển số đơn, nhiều biển số, phát hiện xe, v.v.
- **Vùng quan tâm (RoI)**: Xử lý các khu vực cụ thể trong hình ảnh
- **Hỗ trợ đa quốc gia**: Hỗ trợ các định dạng biển số khác nhau (KR, JP, VN, v.v.)

### 6. Phương pháp xử lý hình ảnh

Ví dụ này minh họa ba cách khác nhau để xử lý hình ảnh:

1. **read_image_file**: Xử lý tệp trực tiếp bằng `anpr_read_file` (phương pháp nhanh nhất)
2. **read_encoded_image**: Truyền byte hình ảnh được mã hóa với định dạng pixel "encoded"
3. **read_pixel_buffer**: Giải mã hình ảnh thành dữ liệu pixel RGB thô bằng StumpyPNG/StumpyJPEG, sau đó truyền vào `anpr_read_pixels`
   - Tệp PNG: Giải mã bằng StumpyPNG
   - Tệp JPEG: Giải mã bằng StumpyJPEG, tự động chuyển sang định dạng encoded nếu giải mã thất bại

Để chuyển đổi giữa các phương pháp, sửa đổi biến `anpr_func` trong mã:

```crystal
# Chọn một trong các tùy chọn sau:
anpr_func = ->read_image_file(TSANPR, String, String, String)
# anpr_func = ->read_encoded_image(TSANPR, String, String, String)
# anpr_func = ->read_pixel_buffer(TSANPR, String, String, String)
```

**Lưu ý**: Hàm `read_pixel_buffer` tự động xử lý lỗi giải mã JPEG bằng cách chuyển sang định dạng encoded, đảm bảo hoạt động ổn định trên nhiều biến thể JPEG khác nhau.

### 7. Tham khảo API

#### Lớp TSANPR

Lớp `TSANPR` cung cấp các phương thức sau:

**Constructor:**

- `TSANPR.new(library_path : String)`: Khởi tạo với đường dẫn thư viện gốc

**Phương thức cốt lõi:**

- `anpr_initialize(mode : String) : String`: Khởi tạo engine ANPR
- `anpr_read_file(img_file_name : String, output_format : String, options : String) : String`: Xử lý tệp hình ảnh
- `anpr_read_pixels(pixels : UInt8*, width : UInt64, height : UInt64, stride : Int64, pixel_format : String, output_format : String, options : String) : String`: Xử lý dữ liệu pixel

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

- Đảm bảo trình biên dịch Crystal được cài đặt đúng cách
- Kiểm tra đường dẫn thư viện tồn tại trong quá trình biên dịch
- Sử dụng `crystal build --release` cho bản dựng được tối ưu hóa

**Vấn đề cụ thể theo nền tảng:**

- **Windows**: Đảm bảo Visual C++ Redistributable đã được cài đặt
- **Linux**: Cài đặt các thư viện hệ thống cần thiết và đảm bảo quyền thư viện là chính xác
