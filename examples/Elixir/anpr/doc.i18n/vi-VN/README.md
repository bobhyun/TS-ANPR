[English](../../README.md) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ Elixir

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Elixir/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Elixir/anpr)

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
  └── Elixir
      └── anpr                   # thư mục dự án
         ├── c_src               # mã nguồn C NIF
         │   └── tsanpr_nif.c
         ├── lib                 # mã nguồn Elixir
         │   ├── anpr.ex         # module chính
         │   └── tsanpr.ex       # module wrapper NIF
         ├── priv                # thư viện NIF (kết quả build)
         │   └── tsanpr_nif.dll/.so
         ├── mix.exs             # cấu hình dự án Mix
         ├── Makefile            # cấu hình build (Linux)
         ├── Makefile.win        # cấu hình build (Windows, nmake)
         ├── build_nif.bat       # script build (Windows)
         └── _build              # đầu ra build của Mix (được tạo)
  ```

### 2. Yêu cầu tiên quyết

1. Cài đặt Elixir (khuyến nghị phiên bản 1.12 trở lên)

   **Windows:**

   ```sh
   # Sử dụng Chocolatey
   choco install elixir

   # Hoặc tải xuống từ https://elixir-lang.org/install.html#windows
   ```

   **Linux:**

   ```sh
   # Ubuntu/Debian
   sudo apt-get install elixir

   # Hoặc sử dụng asdf (khuyến nghị)
   asdf plugin add elixir
   asdf install elixir latest
   ```

2. Xác minh cài đặt

   ```sh
   elixir --version
   ```

### 3. Build và chạy

1. Điều hướng đến thư mục ví dụ Elixir

   ```sh
   cd Elixir/anpr
   ```

2. Build NIF (thư viện native)

   **Windows:**

   Mở "x64 Native Tools Command Prompt for VS 2022" để `cl`/`link` có trong PATH, sau đó:

   ```cmd
   build_nif.bat
   ```

   Thao tác này sẽ:
   - Tự động phát hiện cài đặt Erlang
   - Biên dịch `c_src/tsanpr_nif.c` bằng MSVC
   - Tạo `priv/tsanpr_nif.dll`

   Lưu ý: Nếu `mix compile` gọi `nmake` qua `elixir_make` trên Windows, `Makefile.win` sẽ được sử dụng và gọi tiếp `build_nif.bat` để tránh vấn đề quoting trên Windows.

   **Linux:**

   ```sh
   make priv/tsanpr_nif.so
   ```

   - Biên dịch `c_src/tsanpr_nif.c` bằng gcc
   - Tạo `priv/tsanpr_nif.so`

3. Cài đặt phụ thuộc và biên dịch ứng dụng Elixir

   ```sh
   mix deps.get
   mix compile
   ```

4. Chạy ví dụ

   ```sh
   # Chạy với Mix
   mix run -e "ANPR.main()"

   # Hoặc với iex (tương tác)
   iex -S mix
   iex> ANPR.main()
   ```

### 4. Ghi chú

- Triển khai Elixir này cung cấp cùng chức năng như các ví dụ ngôn ngữ khác
- Sử dụng NIF (Native Implemented Functions) của Elixir để giao tiếp với thư viện TSANPR gốc
- Thiết kế chịu lỗi và mô hình actor của Elixir làm cho nó xuất sắc cho các hệ thống phân tán
- Hỗ trợ đa nền tảng cho Windows và Linux
- Lưu ý: Đây là triển khai NIF đơn giản hóa cho mục đích demo

Ghi chú triển khai bổ sung:
- Trên Windows, `ANPR.get_engine_file_name/0` chọn `examples/bin/windows-x86_64/tsanpr.dll` khi BEAM 64-bit (`:erlang.system_info(:wordsize) == 8`), nếu không sẽ dùng `windows-x86`.
- Module NIF được đăng ký là `Elixir.TSANPR` trong `c_src/tsanpr_nif.c` và được tải theo tên cơ sở (`tsanpr_nif`) trong `lib/tsanpr.ex` để tự động chọn phần mở rộng phù hợp với nền tảng.

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

Module `TSANPR` cung cấp các hàm sau:

**Khởi tạo:**

- `TSANPR.new(library_path)`: Khởi tạo với đường dẫn thư viện gốc

**Hàm cốt lõi:**

- `TSANPR.anpr_initialize(tsanpr, mode)`: Khởi tạo engine ANPR
- `TSANPR.anpr_read_file(tsanpr, img_file_name, output_format, options)`: Xử lý tệp hình ảnh
- `TSANPR.anpr_read_pixels(tsanpr, pixels, width, height, stride, pixel_format, output_format, options)`: Xử lý dữ liệu pixel

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
