[English](../../README.md) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ Erlang

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Erlang/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Erlang/anpr)

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
  └── Erlang
      └── anpr                   # thư mục dự án
         ├── c_src               # tệp nguồn C NIF
         │   └── tsanpr_nif.c
         ├── src                 # tệp nguồn Erlang
         │   ├── anpr_app.erl    # Hành vi ứng dụng
         │   ├── anpr_sup.erl    # Hành vi Supervisor
         │   ├── anpr.erl        # Module chính
         │   ├── tsanpr.erl      # Module bao bọc NIF
         │   └── anpr.app.src    # Tệp tài nguyên ứng dụng
         ├── priv                # Thư viện NIF đã biên dịch
         │   └── tsanpr_nif.dll/.so
         ├── rebar.config        # Cấu hình rebar3
         └── _build              # Đầu ra build của rebar3 (được tạo)
            └── default
               └── lib
                  └── anpr
                     ├── ebin    # Tệp .beam đã biên dịch
                     └── priv    # Thư viện NIF
  ```

### 2. Yêu cầu tiên quyết

1. Cài đặt Erlang/OTP (khuyến nghị phiên bản 24 trở lên)

   **Windows:**

   ```sh
   # Tải từ https://www.erlang.org/downloads
   # Hoặc sử dụng Chocolatey
   choco install erlang
   ```

   **Linux:**

   ```sh
   # Ubuntu/Debian
   sudo apt-get update
   sudo apt-get install -y erlang

   # Oracle Linux / RHEL / CentOS (8/9+)
   sudo dnf install -y erlang || sudo yum install -y erlang
   # Gợi ý: Nếu không tìm thấy gói, hãy bật EPEL hoặc dùng repo Erlang Solutions:
   # https://www.erlang.org/downloads

   # Hoặc dùng kerl (khuyến nghị)
   curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
   chmod a+x kerl
   ./kerl build 24.3 24.3
   ./kerl install 24.3 ~/erlang/24.3
   # Kích hoạt trong shell hiện tại
   . ~/erlang/24.3/activate
   # Hủy kích hoạt: deactivate
   ```

2. Cài đặt rebar3

   **Windows:**

   Tải xuống rebar3 escript mới nhất từ GitHub:
   ```powershell
   # Tải xuống rebar3 mới nhất (tương thích với Erlang/OTP 28+)
   Invoke-WebRequest -Uri "https://github.com/erlang/rebar3/releases/latest/download/rebar3" -OutFile "rebar3"
   
   # Đặt rebar3 trong thư mục dự án hoặc di chuyển vào thư mục trong PATH
   ```

   **Lưu ý:** Trên Windows, rebar3 phải được chạy với `escript rebar3 <lệnh>` (không phải chỉ `rebar3 <lệnh>`).

   **Linux:**

   ```sh
   # Ubuntu/Debian
   sudo apt-get install rebar3
   
   # Hoặc tải phiên bản mới nhất từ GitHub
   wget https://github.com/erlang/rebar3/releases/latest/download/rebar3
   chmod +x rebar3
   sudo mv rebar3 /usr/local/bin/
   ```

3. Cài đặt trình biên dịch C (cần thiết cho việc build NIF)

   **Windows:**
   - Cài đặt Visual Studio với công cụ phát triển C++, hoặc
   - Cài đặt MinGW-w64: `choco install mingw`

   **Linux:**
   ```sh
   # Ubuntu/Debian
   sudo apt-get install build-essential

   # Fedora/CentOS
   sudo yum groupinstall "Development Tools"
   ```

4. Xác minh cài đặt

   ```sh
   erl -version
   rebar3 version
   gcc --version  # cl.exe trên Windows khi sử dụng Visual Studio
   ```

### 3. Build và chạy

1. Chuyển đến thư mục ví dụ Erlang

   ```sh
   cd examples/Erlang/anpr
   ```

2. Build thư viện NIF (Native Implemented Function)

   **Windows:**

   Mở "x64 Native Tools Command Prompt for VS" (hoặc chạy vcvars64.bat), sau đó:
   ```cmd
   build_nif.bat
   ```

   Sẽ thực hiện:
   - Tự động phát hiện cài đặt Erlang
   - Biên dịch `c_src/tsanpr_nif.c` bằng MSVC
   - Tạo `priv/tsanpr_nif.dll`

   **Linux:**

   ```sh
   make priv/tsanpr_nif.so
   ```

   Sẽ thực hiện:
   - Biên dịch `c_src/tsanpr_nif.c` bằng gcc
   - Tạo `priv/tsanpr_nif.so` với liên kết libdl thích hợp

3. Build ứng dụng Erlang

   **Windows (sử dụng lệnh escript):**
   ```cmd
   escript rebar3 compile
   ```

   **Linux:**
   ```sh
   rebar3 compile
   ```

   Sẽ thực hiện:
   - Biên dịch các module Erlang (`anpr.erl`, `tsanpr.erl`) vào `_build/default/lib/anpr/ebin/`
   - Sao chép thư viện NIF từ `priv/` sang `_build/default/lib/anpr/priv/`

4. Chạy ứng dụng

   ```sh
   # Chế độ không tương tác
   erl -pa _build/default/lib/anpr/ebin -noshell -eval "anpr:main()" -s init stop

   # Chế độ tương tác
   erl -pa _build/default/lib/anpr/ebin
   1> anpr:main().
   ```

### 4. Ghi chú

- Triển khai Erlang này cung cấp cùng chức năng như các ví dụ ngôn ngữ khác
- Sử dụng NIF (Native Implemented Functions) của Erlang để giao tiếp với thư viện TSANPR gốc
- Tính chịu lỗi và mô hình đồng thời của Erlang làm cho nó lý tưởng cho các hệ thống có tính khả dụng cao
- Hỗ trợ đa nền tảng cho Windows và Linux
- Lưu ý: Đây là triển khai NIF đơn giản hóa cho mục đích demo

### 5. Tính năng

- **Nhận dạng dựa trên tệp**: Xử lý trực tiếp các tệp hình ảnh
- **Xử lý hình ảnh được mã hóa**: Xử lý dữ liệu hình ảnh được mã hóa (JPEG, PNG, v.v.)
- **Xử lý bộ đệm pixel**: Xử lý dữ liệu pixel thô (triển khai đơn giản hóa)
- **Nhiều định dạng đầu ra**: Hỗ trợ đầu ra văn bản, JSON, YAML, XML và CSV
- **Nhiều chế độ nhận dạng**: Biển số đơn, nhiều biển số, phát hiện xe, v.v.
- **Vùng quan tâm (RoI)**: Xử lý các khu vực cụ thể trong hình ảnh
- **Hỗ trợ đa quốc gia**: Hỗ trợ các định dạng biển số khác nhau (KR, JP, VN, v.v.)

### 6. Tham khảo API

#### Module tsanpr

Module `tsanpr` cung cấp các hàm sau:

**Khởi tạo:**

- `tsanpr:new(LibraryPath)`: Khởi tạo với đường dẫn thư viện gốc

**Hàm cốt lõi:**

- `tsanpr:anpr_initialize(Tsanpr, Mode)`: Khởi tạo engine ANPR
- `tsanpr:anpr_read_file(Tsanpr, ImgFileName, OutputFormat, Options)`: Xử lý tệp hình ảnh
- `tsanpr:anpr_read_pixels(Tsanpr, Pixels, Width, Height, Stride, PixelFormat, OutputFormat, Options)`: Xử lý dữ liệu pixel

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

**Vấn đề NIF:**

- Ví dụ này sử dụng phương pháp NIF đơn giản hóa cho mục đích demo
- Để sử dụng trong sản xuất, bạn cần triển khai NIF C thích hợp
- Đảm bảo các header phát triển Erlang đã được cài đặt

**Vấn đề cụ thể theo nền tảng:**

- **Windows**: Đảm bảo Visual C++ Redistributable đã được cài đặt
- **Linux**: Cài đặt các thư viện hệ thống cần thiết và đảm bảo quyền thư viện là chính xác
