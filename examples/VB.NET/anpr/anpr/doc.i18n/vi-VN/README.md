[English](../../) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ Visual Basic(.NET)

https://github.com/bobhyun/TS-ANPR/tree/main/examples/VB.NET/anpr

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
  │   ├─── windows-x86_64        # engine directory for Windows (x86_64)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86           # engine directory for Windows (x86)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64           # engine directory for Linux (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # engine directory for Linux (arm64)
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                       # image directory
  └── VB.NET
      └── anpr                  # source directory
          ├── anpr.sln
          └── anpr
              ├── anpr.vbproj
              ├── Program.vb
              ├── TSANPR.vb
              └── bin           # compiled executable directory
                 ├── Release
                 │    └── net8.0
                 │        ├── anpr.exe
                 │        ├── anpr.dll
                 │        └──OpenCvSharp.dll
                 └── Debug
                      └── net8.0
                          ├── anpr.exe
                          ├── anpr.dll
                          └──OpenCvSharp.dll
  ```

### 2. Cài đặt phụ thuộc

- Windows (NuGet)
  ```sh
  dotnet add package OpenCvSharp4
  dotnet add package OpenCvSharp4.runtime.win
  ```
- x86 Linux
  ```sh
  dotnet add package OpenCvSharp4
  dotnet add package OpenCvSharp4.runtime.linux-x64
  ```
- ARM Linux
  ```sh
  dotnet add package OpenCvSharp4
  dotnet add package OpenCvSharp4.runtime.linux-arm
  ```
