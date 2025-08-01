[English](../../) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ Go

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Go/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Go/anpr)

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
  └── Go
      └── anpr                   # source directory
         ├── anpr                # compiled executable for Windows
         ├── anpr.exe            # compiled executable for Linux
         ├── go.mod
         ├── main.go
         └── tsanpr
            ├── tsanpr_windows.go
            └── tsanpr_linux.go
  ```

### 2. Biên dịch và chạy

#### 2.1 Windows

1. Khởi tạo module

   ```sh
   go mod tidy
   ```

2. Cài đặt phụ thuộc

   - Tham khảo hướng dẫn cài đặt chính thức của GoCV. (https://gocv.io/getting-started/windows/)

3. Cách biên dịch

   ```sh
   go build -o anpr.exe main.go
   ```

4. Cách chạy

   ```sh
   .\anpr.exe
   ```

#### 2.2. Linux

1. Khởi tạo module

   ```sh
   go mod tidy
   ```

2. Cài đặt phụ thuộc
   - Debian / Ubuntu Linux
     ```sh
     sudo apt install -y build-essential g++ libopencv-dev
     ```
   - Oracle Linux / RedHat (RHEL) / CentOS
     ```sh
     sudo dnf install -y epel-release gcc gcc-c++ opencv opencv-devel
     ```
3. Cách biên dịch

   ```sh
   go build -o anpr main.go
   ```

4. Cách chạy

   ```sh
   ./anpr
   ```
