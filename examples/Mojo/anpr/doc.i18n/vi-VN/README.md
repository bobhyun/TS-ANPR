[English](../../README.md) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ Mojo

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Mojo/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Mojo/anpr)

### 1. Sao chép các tệp Engine

_**[Lưu ý]** Mojo hiện chỉ hỗ trợ Linux. Trong ví dụ này, tệp engine được giải nén vào thư mục examples/bin/ để chia sẻ với các ví dụ khác. Tuy nhiên, đối với triển khai thực tế, tệp engine thường được sao chép vào thư mục nơi tệp thực thi của ứng dụng được đặt._

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
  │   ├── linux-x86_64           # thư mục engine cho Linux (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # thư mục engine cho Linux (arm64)
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # thư mục hình ảnh
  └── Mojo
      └── anpr                   # thư mục dự án
          ├── pixi.toml
          └── src                # thư mục nguồn
              ├── anpr.mojo
              ├── webcam.mojo
              └── tsanpr.mojo
  ```

### 2. Cách chạy

1. Cài đặt Mojo SDK

   - Cài đặt pixi (trình quản lý gói)
     ```sh
     curl -fsSL https://pixi.sh/install.sh | sh
     source ~/.bashrc  # hoặc ~/.zshrc
     ```

   - Cài đặt Mojo bằng pixi (đã cấu hình trong pixi.toml)
     ```sh
     cd examples/Mojo/anpr
     pixi install
     ```

   Để biết thêm chi tiết, xem: https://docs.modular.com/mojo/manual/install/

2. Chạy `anpr`

   ```sh
   pixi run anpr
   ```

3. Chạy `webcam`

   ```sh
   pixi run webcam
   ```
