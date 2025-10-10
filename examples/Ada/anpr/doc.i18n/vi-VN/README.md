[English](../../README.md) | [한국어](../ko-KR/README.md) | [日本語](../ja-JP/README.md) | Tiếng Việt

# Ví dụ Ada

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Ada/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Ada/anpr)

### 1. Sao chép tệp Engine

_**[Lưu ý]** Trong ví dụ này, tệp engine được giải nén vào thư mục `examples/bin/` để chia sẻ với các ví dụ khác. Tuy nhiên, khi triển khai thực tế, tệp engine thường được sao chép vào thư mục chứa tệp thực thi của ứng dụng._

- Windows x86 64-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86_64`
  ```sh
  tar xvf tsanpr*-windows-x86_64.tar.xz
  ```
- Windows x86 32-bit
  Giải nén tệp engine vào thư mục `examples/bin/windows-x86`
  ```sh
  tar xvf tsanpr*-windows-x86.tar.xz
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
  └── Ada
      └── anpr                   # thư mục dự án
         ├── bin                 # thư mục tệp thực thi
         ├── obj                 # thư mục tệp đối tượng
         ├── src                 # thư mục mã nguồn
         │   ├── anpr.adb
         │   ├── tsanpr.ads
         │   ├── tsanpr-windows.adb
         │   └── tsanpr-unix.adb
         ├── anpr.gpr
         ├── alire.toml
         ├── compile.bat
         ├── compile.sh
         └── Makefile
  ```

### 2. Xây dựng và Chạy

#### 2.1 Sử dụng Alire (Khuyến nghị)

[Alire](https://alire.ada.dev/) là trình quản lý gói hiện đại cho Ada, tự động quản lý toolchain và các phụ thuộc.

1. Cài đặt Alire

   **Windows:**

   - Tải xuống từ [https://alire.ada.dev](https://alire.ada.dev)
   - Giải nén và thêm `alr` vào PATH

   **Linux:**

   **Khuyến nghị: Cài đặt thủ công (tất cả bản phân phối)**

   Tải phiên bản mới nhất từ [Alire Releases](https://github.com/alire-project/alire/releases):

   ```sh
   # Tải xuống bản phát hành mới nhất (kiểm tra trang phát hành để biết phiên bản hiện tại)
   wget https://github.com/alire-project/alire/releases/download/v2.1.0/alr-2.1.0-bin-x86_64-linux.zip
   unzip alr-2.1.0-bin-x86_64-linux.zip
   sudo mv bin/alr /usr/local/bin/
   ```

   Hoặc tải từ [https://alire.ada.dev](https://alire.ada.dev) và thêm vào PATH

   **Thay thế: Sử dụng trình quản lý gói (có thể là phiên bản cũ)**

   - Debian/Ubuntu:
     ```sh
     sudo apt-get update
     sudo apt-get install alire
     ```
   - Fedora/RHEL/CentOS:
     ```sh
     sudo dnf install alire
     ```

   **Lưu ý:** Nếu gặp lỗi `Unexpected property count: 0` với phiên bản cài từ trình quản lý gói, vui lòng sử dụng phương pháp cài đặt thủ công ở trên để cài phiên bản mới nhất.

2. Xây dựng

   ```sh
   alr build
   ```

   Ở lần xây dựng đầu tiên, Alire sẽ tự động tải xuống GNAT compiler và GPRbuild. Chỉ cần nhấn Enter khi được nhắc.

3. Chạy

   ```sh
   alr run
   ```

   Hoặc chạy trực tiếp:

   ```sh
   bin/anpr
   ```

#### 2.2 Sử dụng GNAT/GPRbuild (Phương pháp truyền thống)

##### 2.2.1 Windows

1. Cài đặt GNAT

   - Tải xuống và cài đặt [GNAT Community](https://www.adacore.com/download)
   - Thêm thư mục bin của GNAT vào PATH

2. Cách xây dựng

   ```cmd
   compile.bat
   ```

   Hoặc sử dụng GPRbuild:

   ```cmd
   gprbuild -p -P anpr.gpr -XOS=Windows_NT
   ```

3. Cách chạy

   Để hiển thị ký tự UTF-8 đúng (khuyến nghị cho ký tự non-ASCII):

   ```cmd
   chcp 65001
   bin\anpr.exe
   ```

   Hoặc không dùng mã hóa UTF-8:

   ```cmd
   bin\anpr.exe
   ```

##### 2.2.2 Linux

1. Cài đặt phụ thuộc

   - Debian / Ubuntu Linux

     ```sh
     sudo apt-get update
     sudo apt-get install gnat gprbuild
     ```

   - Oracle Linux / RedHat (RHEL) / CentOS

     ```sh
     sudo yum install gcc-gnat gprbuild
     ```

   - Fedora

     ```sh
     sudo dnf install gcc-gnat gprbuild
     ```

2. Cách xây dựng

   ```sh
   chmod +x compile.sh
   ./compile.sh
   ```

   Hoặc sử dụng GPRbuild:

   ```sh
   gprbuild -p -P anpr.gpr -XOS=UNIX
   ```

   Hoặc sử dụng Make:

   ```sh
   make
   ```

3. Cách chạy

   ```sh
   bin/anpr
   ```

   **Lưu ý về mã hóa ký tự:**
   Hệ thống Linux mặc định hỗ trợ UTF-8 nên các ký tự tiếng Việt sẽ hiển thị đúng. Nếu gặp vấn đề, hãy đảm bảo terminal được thiết lập với mã hóa UTF-8.
