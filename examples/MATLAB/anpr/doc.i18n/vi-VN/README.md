[English](../../README.md) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ MATLAB/Octave

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/MATLAB/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/MATLAB/anpr)

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
  └── MATLAB
      └── anpr                   # thư mục dự án
         ├── src                 # thư mục nguồn
         │   ├── anpr.m          # script ví dụ chính
         │   ├── TSANPR.m        # lớp wrapper TSANPR
         │   └── mex             # thư mục nguồn MEX
         │       ├── tsanpr_mex.c    # tệp nguồn MEX
         │       └── build_mex.m     # script build MEX
         └── doc.i18n            # tài liệu dịch
  ```

### 2. Yêu cầu tiên quyết

#### Tùy chọn A: MATLAB (Thương mại)

1. Cài đặt MATLAB (khuyến nghị R2018b trở lên)

   **Windows:**
   - Tải và cài đặt MATLAB từ https://www.mathworks.com/downloads/
   - Yêu cầu giấy phép hợp lệ

   **Linux:**
   - Tải và cài đặt MATLAB từ https://www.mathworks.com/downloads/
   - Yêu cầu giấy phép hợp lệ

2. Cấu hình trình biên dịch C
   ```matlab
   mex -setup
   ```

3. Xác minh cài đặt
   ```matlab
   version
   mex -setup
   ```

#### Tùy chọn B: GNU Octave (Miễn phí, Mã nguồn mở)

GNU Octave là phần mềm thay thế miễn phí tương thích phần lớn với MATLAB.

**Windows:**

```cmd
# Sử dụng winget
winget install GNU.Octave

# Hoặc tải từ https://octave.org/download
```

**Linux (Ubuntu/Debian):**

```sh
sudo apt-get update
sudo apt-get install -y octave octave-image liboctave-dev
```

**Linux (Fedora/RHEL):**

```sh
sudo dnf install -y octave octave-image octave-devel
```

Xác minh cài đặt:
```sh
octave --version
```

### 3. Cách build MEX

Tệp MEX được tự động build khi chạy lần đầu. Để build thủ công:

#### Với MATLAB

```matlab
cd examples/MATLAB/anpr/src/mex
build_mex
```

#### Với GNU Octave

```sh
cd examples/MATLAB/anpr/src/mex
octave --eval "build_mex"
```

### 4. Cách chạy

#### Với MATLAB

1. Điều hướng đến thư mục nguồn
   ```sh
   cd examples/MATLAB/anpr/src
   ```

2. Khởi động MATLAB và chạy ví dụ
   ```matlab
   % Chạy ví dụ ANPR chính
   anpr
   ```

#### Với GNU Octave

**Windows:**

```cmd
cd examples\MATLAB\anpr\src
octave --eval "anpr"
```

**Linux:**

```sh
cd examples/MATLAB/anpr/src
octave --eval "anpr"
```

Hoặc chạy tương tác:
```sh
octave
```
```octave
cd examples/MATLAB/anpr/src
anpr
```

### 5. Sử dụng tương tác

```matlab
% Thêm đường dẫn
addpath('mex');

% Khởi tạo TSANPR
engine_path = '../../../bin/windows-x86_64/tsanpr.dll';  % Windows
% engine_path = '../../../bin/linux-x86_64/libtsanpr.so';  % Linux

tsanpr = TSANPR(engine_path);

% Khởi tạo engine
error_msg = tsanpr.anpr_initialize('text;country=KR');
if ~isempty(error_msg)
    fprintf('Lỗi: %s\n', error_msg);
end

% Xử lý hình ảnh
result = tsanpr.anpr_read_file('../../../img/KR/licensePlate.jpg', 'json', '');
fprintf('Kết quả: %s\n', result);
```

### 6. Ghi chú

- Triển khai này sử dụng tệp MEX để tích hợp thư viện gốc
- Tệp MEX cung cấp giao diện nhất quán cho cả MATLAB và Octave
- Tệp MEX được tự động build khi chạy lần đầu nếu không có sẵn
- Hỗ trợ đa nền tảng cho Windows và Linux

### 7. Tính năng

- **Nhận dạng dựa trên tệp**: Xử lý trực tiếp các tệp hình ảnh
- **Xử lý hình ảnh được mã hóa**: Xử lý dữ liệu hình ảnh được mã hóa (JPEG, PNG, v.v.)
- **Xử lý bộ đệm pixel**: Xử lý dữ liệu pixel thô bằng các hàm xử lý hình ảnh
- **Nhiều định dạng đầu ra**: Hỗ trợ đầu ra văn bản, JSON, YAML, XML và CSV
- **Nhiều chế độ nhận dạng**: Biển số đơn, nhiều biển số, phát hiện xe, v.v.
- **Vùng quan tâm (RoI)**: Xử lý các khu vực cụ thể trong hình ảnh
- **Hỗ trợ đa quốc gia**: Hỗ trợ các định dạng biển số khác nhau (KR, JP, VN, v.v.)

### 8. Tham khảo API

#### Lớp TSANPR

**Constructor:**
- `TSANPR(library_path)`: Khởi tạo với đường dẫn thư viện gốc

**Phương thức cốt lõi:**
- `anpr_initialize(mode)`: Khởi tạo engine ANPR
- `anpr_read_file(img_file_name, output_format, options)`: Xử lý tệp hình ảnh
- `anpr_read_pixels(pixels, width, height, stride, pixel_format, output_format, options)`: Xử lý dữ liệu pixel

**Phương thức tĩnh:**
- `TSANPR.isOctave()`: Kiểm tra xem có đang chạy trong GNU Octave không

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

### 9. Khắc phục sự cố

**Vấn đề build MEX:**
- Đảm bảo trình biên dịch C đã được cài đặt
  - **MATLAB**: Chạy `mex -setup` để cấu hình
  - **Octave Windows**: MinGW được bao gồm trong cài đặt Octave
  - **Octave Linux**: Cài đặt `liboctave-dev` hoặc `octave-devel`
- Kiểm tra tính khả dụng của trình biên dịch: `mex -setup` (MATLAB) hoặc `mkoctfile --version` (Octave)

**Vấn đề tải thư viện:**
- Đảm bảo đường dẫn thư viện TSANPR là chính xác
- Kiểm tra tất cả các phụ thuộc hệ thống đã được cài đặt
- Trên Linux, đảm bảo `LD_LIBRARY_PATH` bao gồm thư mục engine

**Vấn đề cụ thể theo nền tảng:**
- **Windows**: Đảm bảo Visual C++ Redistributable đã được cài đặt
- **Linux**: Cài đặt các thư viện hệ thống cần thiết và đảm bảo quyền thư viện là chính xác
