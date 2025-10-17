[English](../../) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# Visual Basic(.NET) サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/VB.NET/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/VB.NET/anpr)

### 1. エンジンファイルのコピー

_**【参考】** この例では、他のサンプルとエンジンファイルを共有するために `examples/bin/` ディレクトリに展開しますが、実際の配布時には通常、アプリケーションの実行ファイルがあるディレクトリにエンジンファイルをコピーします。_

- Windows x86 64 ビット
  エンジンファイルを `examples/bin/windows-x86_64` ディレクトリに展開
  ```sh
  7z x tsanpr*-windows-x86_64.7z
  ```
- Windows x86 32 ビット
  エンジンファイルを `examples/bin/windows-x86` ディレクトリに展開
  ```sh
  7z x tsanpr*-windows-x86.7z
  ```
- Linux x86 64 ビット
  エンジンファイルを `examples/bin/linux-x86_64` ディレクトリに展開
  ```sh
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- Linux ARM 64 ビット
  エンジンファイルを `examples/bin/linux-aarch64` ディレクトリに展開
  ```sh
  tar xvf tsanpr*-linux-aarch64.tar.xz
  ```
- ディレクトリ構成
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

### 2. 依存関係のインストール

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
