[English](../../) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ Rust

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Rust/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Rust/anpr)

### 1. Sao chép tệp Engine

_**[Lưu ý]** Trong ví dụ này, tệp engine được giải nén vào thư mục examples/bin/ để chia sẻ với các ví dụ khác. Tuy nhiên, khi triển khai thực tế, tệp engine thường được sao chép vào thư mục chứa tệp thực thi của ứng dụng._

- Windows x86 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86_64`
  ```sh
  unzip tsanpr*-windows-x86_64.zip
  ```
- Windows x86 32-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86`
  ```sh
  unzip tsanpr*-windows-x86.zip
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
  │   ├─── windows-x86_64       # engine directory for Windows (x86_64)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86          # engine directory for Windows (x86)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64          # engine directory for Linux (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64         # engine directory for Linux (arm64)
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                       # image directory
  └── Rust
      └── anpr                  # source directory
          ├── Cargo.toml
          ├── src
          |   ├── main.rs
          |   └── tsanpr.rs
          └── target            # compiled executables
              ├── debug
              |   └── anpr.exe
              └── release
                  └── anpr.exe
  ```

### 2. Biên dịch và chạy

```sh
cargo build
cargo run
```
