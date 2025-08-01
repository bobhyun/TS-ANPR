[English](../../) | [한국어](../ko-KR/) | [日本語](../ja-JP/) | Tiếng Việt

# Ví dụ Haskell

https://github.com/bobhyun/TS-ANPR/tree/main/examples/Haskell/anpr

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
  └── Haskell
      └── anpr                  # source directory
          ├── anpr.cabal
          ├── Setup.hs
          ├── stack.yaml
          ├── app
          │   └── Main.hs
          ├── src
          │   └── TSANPR.hs
          └── .stack-work       # compiled executableS
                └── install
                  ├── x86-windows
                  │   └── <ghc-version>
                  │         └── <hash>
                  │             └── bin
                  │                 └── anpr.exe
                  ├── linux-x86-64
                  │   └── <ghc-version>
                  │         └── <hash>
                  │             └── bin
                  │                 └── anpr
                  │
                  └── linux-aarch64
                      └── <ghc-version>
                            └── <hash>
                                └── bin
                                    └── anpr
  ```

### 2. Cách xây dựng và chạy

```sh
# build
stack build

# Run
stack run
```
