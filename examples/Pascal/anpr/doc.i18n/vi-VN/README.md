[English](../../README.md) | [한국어](../ko-KR/README.md) | [日本語](../ja-JP/README.md) | Tiếng Việt

# Ví dụ Pascal

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Pascal/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Pascal/anpr)

### 1. Sao chép tệp Engine

_**[Lưu ý]** Trong ví dụ này, tệp engine được giải nén vào thư mục `examples/bin/` để chia sẻ với các ví dụ khác. Tuy nhiên, khi triển khai thực tế, tệp engine thường được sao chép vào thư mục chứa tệp thực thi của ứng dụng._

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
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- Linux arm 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/linux-aarch64`
  ```sh
  tar xvf tsanpr*-linux-aarch64.tar.xz
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
  └── Pascal
      └── anpr                   # thư mục dự án
         ├── src                 # thư mục mã nguồn chính
         │   └── anpr.pas        # chương trình chính
         ├── units               # thư mục unit
         │   ├── tsanpr.pas      # unit TSANPR
         │   └── stb_image.pas   # bindings stb_image
         ├── lib                 # mã nguồn thư viện ngoài
         │   ├── stb_image.h     # header stb_image
         │   └── stb_image_lib.c # mã nguồn thư viện chia sẻ stb_image
         ├── bin                 # thư mục đầu ra (được tạo khi build)
         ├── compile.bat
         ├── compile.sh
         └── Makefile
  ```

### 2. Xây dựng và Chạy

#### 2.1 Windows

1. Cài đặt Free Pascal

   - Tải xuống và cài đặt [Free Pascal](https://www.freepascal.org/download.html)
   - Thêm thư mục bin của Free Pascal vào PATH

2. Cách xây dựng

   ```cmd
   compile.bat
   ```

3. Cách chạy

   ```cmd
   cd bin
   anpr.exe
   ```

#### 2.2 Linux

1. Cài đặt phụ thuộc

   - Debian / Ubuntu Linux

     ```sh
     sudo apt-get update
     sudo apt-get install fpc
     ```

   - Oracle Linux / RedHat (RHEL) / CentOS

     ```sh
     sudo yum install fpc
     ```

   - Fedora

     ```sh
     sudo dnf install fpc
     ```

2. Cách xây dựng

   ```sh
   chmod +x compile.sh
   ./compile.sh
   ```

   Hoặc sử dụng Make:

   ```sh
   make
   ```

3. Cách chạy

   ```sh
   cd bin
   ./anpr
   ```
