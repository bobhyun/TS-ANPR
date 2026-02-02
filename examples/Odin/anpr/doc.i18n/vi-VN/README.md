[English](../../README.md) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ Odin

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Odin/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Odin/anpr)

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
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- Cho Linux arm 64-bit
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
  └── Odin
      └── anpr                   # thư mục dự án
          ├── anpr.odin
          ├── tsanpr             # thư mục package tsanpr
          │   └── tsanpr.odin
          ├── stb_image          # thư mục package stb_image
          │   ├── stb_image.odin
          │   ├── stb_image.h
          │   └── stb_image_impl.c
          ├── build.bat
          └── build.sh
  ```

### 2. Cách chạy

1. Cài đặt Odin

   ```sh
   # Windows (sử dụng Scoop)
   scoop install odin

   # macOS (sử dụng Homebrew)
   brew install odin

   # Linux (Arch Linux)
   pacman -S odin

   # Linux (Ubuntu/Debian - build từ source)
   sudo apt install llvm clang
   git clone https://github.com/odin-lang/Odin.git
   cd Odin && make release
   echo 'export PATH=$PATH:$HOME/Odin' >> ~/.bashrc && source ~/.bashrc
   ```

2. Build và chạy `anpr`

   Script build sẽ biên dịch thư viện C stb_image trước, sau đó build dự án Odin.

   ```sh
   cd examples/Odin/anpr

   # Windows (cần Visual Studio hoặc MinGW)
   build.bat
   anpr.exe

   # Linux/macOS (cần cc/gcc/clang)
   chmod +x build.sh
   ./build.sh
   ./anpr
   ```
