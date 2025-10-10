[English](../../README.md) | [한국어](../ko-KR/README.md) | [日本語](../ja-JP/README.md) | Tiếng Việt

# Ví dụ COBOL

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/COBOL/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/COBOL/anpr)

Ví dụ này trình bày cách tích hợp engine TS-ANPR với các ứng dụng COBOL bằng thư viện wrapper C.

## Kiến trúc

Ví dụ COBOL sử dụng kiến trúc phân lớp:

```
Ứng dụng COBOL (anpr.cbl)
         ↓
Thư viện Wrapper C (libtsanpr_cobol.so/.dll)
         ↓
Engine TS-ANPR (libtsanpr.so/tsanpr.dll)
```

- **Ứng dụng COBOL**: Chương trình chính gọi wrapper C
- **Wrapper C**: Cung cấp giao diện thân thiện với COBOL cho engine TS-ANPR
- **Engine TS-ANPR**: Engine nhận dạng biển số xe dựa trên deep learning

### 1. Sao chép tệp Engine

_**[Lưu ý]** Trong ví dụ này, tệp engine được giải nén vào thư mục `examples/bin/` để chia sẻ với các ví dụ khác. Tuy nhiên, khi triển khai thực tế, tệp engine thường được sao chép vào thư mục chứa tệp thực thi của ứng dụng._

- Windows x86 64-bit: Giải nén vào thư mục `examples/bin/windows-x86_64`
  ```sh
  tar xvf tsanpr*-windows-x86_64.tar.xz
  ```
- Windows x86 32-bit: Giải nén vào thư mục `examples/bin/windows-x86`
  ```sh
  tar xvf tsanpr*-windows-x86.tar.xz
  ```
- Linux x86 64-bit: Giải nén vào thư mục `examples/bin/linux-x86_64`
  ```sh
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- Linux arm 64-bit: Giải nén vào thư mục `examples/bin/linux-aarch64`
  ```sh
  tar xvf tsanpr*-linux-aarch64.tar.xz
  ```

- Cấu trúc thư mục
  ```
  examples
  ├── bin
  │   ├── windows-x86_64        # thư mục engine cho Windows (x86_64)
  │   ├── windows-x86           # thư mục engine cho Windows (x86)
  │   ├── linux-x86_64           # thư mục engine cho Linux (x86_64)
  │   └── linux-aarch64          # thư mục engine cho Linux (arm64)
  ├── img                        # thư mục hình ảnh
  └── COBOL
      └── anpr                   # thư mục mã nguồn
         ├── src
         │   ├── c               # tệp nguồn wrapper C
         │   │   ├── tsanpr_cobol.c
         │   │   └── tsanpr_cobol.h
         │   └── cobol           # tệp nguồn COBOL
         │       └── anpr.cbl
         ├── bin                 # thư mục đầu ra build
         ├── compile.bat         # script build Windows
         ├── compile.sh          # script build Linux
         ├── run.bat             # script chạy Windows
         ├── run.sh              # script chạy Linux
         └── Makefile            # tệp build GNU Make
  ```

### 2. Xây dựng và Chạy

#### 2.1 Windows

1. **Cài đặt GnuCOBOL**

   - Tải xuống và cài đặt [GnuCOBOL for Windows](https://sourceforge.net/projects/gnucobol/)
   - Hoặc cài đặt qua [SuperBOL](https://superbol.eu/developers/windows/) (khuyến nghị)
   - Thêm thư mục bin của GnuCOBOL vào PATH

2. **Cài đặt GCC**

   - GCC là cần thiết để build wrapper C
   - Nếu bạn đã cài đặt SuperBOL, MinGW64 GCC đã được bao gồm
   - Nếu không, hãy cài đặt [MinGW-w64](https://www.mingw-w64.org/)

3. **Cách xây dựng**

   ```cmd
   compile.bat
   ```

   Hoặc sử dụng Make:

   ```cmd
   make
   ```

4. **Cách chạy**

   ```cmd
   run.bat
   ```

   Hoặc sử dụng Make:

   ```cmd
   make run
   ```

#### 2.2 Linux

1. **Cài đặt phụ thuộc**

   - Debian / Ubuntu
     ```sh
     sudo apt-get update
     sudo apt-get install gnucobol gcc
     ```
   - Oracle Linux / RHEL / CentOS
     ```sh
     sudo yum install gnucobol gcc
     ```
   - Fedora
     ```sh
     sudo dnf install gnucobol gcc
     ```

2. **Cách xây dựng**

   ```sh
   chmod +x compile.sh
   ./compile.sh
   ```

   Hoặc sử dụng Make:

   ```sh
   make
   ```

3. **Cách chạy**

   ```sh
   chmod +x run.sh
   ./run.sh
   ```

   Hoặc sử dụng Make:

   ```sh
   make run
   ```

## Chương trình COBOL (`src/cobol/anpr.cbl`)

Chương trình minh họa:
- Nhận dạng biển số đơn
- Nhận dạng nhiều biển số (vm)
- Nhiều biển bao gồm xe máy (vmb)
- Phát hiện xung quanh (vms)
- Phát hiện đối tượng (dms, dmsr)
- Phát hiện Vùng Quan Tâm (ROI) (dmsri)

Tất cả các tính năng đều khớp với các ví dụ ở ngôn ngữ khác (C, Python, v.v.).

## Lưu ý về Hiệu suất

- **Windows**: Khởi tạo engine mất khoảng 1 giây
- **WSL/Linux trên Windows filesystem (`/mnt/`)**: Khởi tạo engine có thể mất 5-7 giây do overhead hiệu suất giữa các filesystem
- **Native Linux**: Khởi tạo engine mất khoảng 1-2 giây

Để có hiệu suất tốt hơn trên WSL, hãy sao chép toàn bộ dự án vào filesystem gốc của WSL (ví dụ: `~/`).

## Xử lý Sự cố

### Linux: "libcob: error: module not found"

Lỗi này xảy ra khi GnuCOBOL không thể tìm thấy thư viện wrapper C. Script chạy sử dụng `LD_PRELOAD` để tải thư viện bắt buộc.

**Giải pháp**: Luôn sử dụng `run.sh` hoặc `make run` thay vì chạy binary trực tiếp.

### Windows: Lỗi DLL not found

Đảm bảo rằng:
1. `bin/tsanpr_cobol.dll` tồn tại sau khi biên dịch
2. Bạn đang chạy qua `run.bat` thiết lập `COB_LIBRARY_PATH`

## Giấy phép

Mã ví dụ này được phát hành theo Giấy phép MIT.