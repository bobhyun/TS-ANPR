[English](../README.md) | [日本語](README_ja-JP.md) | [한국어](README_ko-KR.md) | Tiếng Việt

# Ví dụ Ruby

### 1. Sao chép tệp Engine

_**[Lưu ý]** Trong ví dụ này, tệp engine được giải nén vào thư mục examples/bin/ để chia sẻ với các ví dụ khác. Tuy nhiên, khi triển khai thực tế, tệp engine thường được sao chép vào thư mục chứa tệp thực thi của ứng dụng._

- Windows x86 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86_64`
  ```sh
  unzip tsanpr*-windows-x86_64.zip
  ```
- Linux x86 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/linux-x86_64`
  ```sh
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- Linux ARM 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/linux-aarch64`
  ```sh
  tar xvf tsanpr*-linux-aarch64.tar.xz
  ```
- Cấu trúc thư mục
  ```sh
  examples
  ├── bin
  │   ├─── windows-x86_64        # engine directory for windows (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   ├── linux-x86_64           # engine directory for linux (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # engine directory for linux (arm64)
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # image directory
  └── Ruby
      └── anpr                   # source directory
         ├── anpr.rb
         └── lib
            └── tsanpr.rb
  ```

### 2. Cài đặt và chạy chương trình

1. Cài đặt phụ thuộc

   1. Windows

      ```sh
      # Khuyến nghị sử dụng Chocolatey.
      choco install vips
      gem install ffi ruby-vips
      ```

   2. Linux

      ```sh
      # Debian / Ubuntu Linux
      sudo apt-get install -y libvips

      # Oracle Linux / RedHat (RHEL) / CentOS
      sudo yum install vips

      sudo gem install ffi ruby-vips
      ```

2. Cách chạy chương trình

   ```sh
   ruby anpr.rb
   ```
