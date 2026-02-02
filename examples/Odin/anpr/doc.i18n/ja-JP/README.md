[English](../../README.md) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# Odin サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Odin/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Odin/anpr)

### 1. エンジンファイルのコピー

_**[注意]** このサンプルでは、他のサンプルと共有するためにエンジンファイルを examples/bin/ディレクトリに展開します。ただし、実際のデプロイメントでは、通常、アプリケーションの実行ファイルが配置されているディレクトリにエンジンファイルをコピーします。_

- Windows x86 64 ビット用
  エンジンファイルを`examples/bin/windows-x86_64`ディレクトリに展開
  ```sh
  7z x tsanpr*-windows-x86_64.7z
  ```
- Windows x86 32 ビット用
  エンジンファイルを`examples/bin/windows-x86`ディレクトリに展開
  ```sh
  7z x tsanpr*-windows-x86.7z
  ```
- Linux x86 64 ビット用
  エンジンファイルを`examples/bin/linux-x86_64`ディレクトリに展開
  ```sh
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- Linux arm 64 ビット用
  エンジンファイルを`examples/bin/linux-aarch64`ディレクトリに展開
  ```sh
  tar xvf tsanpr*-linux-aarch64.tar.xz
  ```
- ディレクトリ構造
  ```sh
  examples
  ├── bin
  │   ├─── windows-x86_64        # Windows (x86_64)用エンジンディレクトリ
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86           # Windows (x86)用エンジンディレクトリ
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64           # Linux (x86_64)用エンジンディレクトリ
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # Linux (arm64)用エンジンディレクトリ
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # 画像ディレクトリ
  └── Odin
      └── anpr                   # プロジェクトディレクトリ
          ├── anpr.odin
          ├── tsanpr             # tsanprパッケージディレクトリ
          │   └── tsanpr.odin
          ├── stb_image          # stb_imageパッケージディレクトリ
          │   ├── stb_image.odin
          │   ├── stb_image.h
          │   └── stb_image_impl.c
          ├── build.bat
          └── build.sh
  ```

### 2. 実行方法

1. Odin のインストール

   ```sh
   # Windows (Scoop を使用)
   scoop install odin

   # macOS (Homebrew を使用)
   brew install odin

   # Linux (Arch Linux)
   pacman -S odin

   # Linux (Ubuntu/Debian - ソースからビルド)
   sudo apt install llvm clang
   git clone https://github.com/odin-lang/Odin.git
   cd Odin && make release
   echo 'export PATH=$PATH:$HOME/Odin' >> ~/.bashrc && source ~/.bashrc
   ```

2. `anpr` をビルドして実行

   ビルドスクリプトは stb_image C ライブラリをコンパイルしてから Odin プロジェクトをビルドします。

   ```sh
   cd examples/Odin/anpr

   # Windows (Visual Studio または MinGW が必要)
   build.bat
   anpr.exe

   # Linux/macOS (cc/gcc/clang が必要)
   chmod +x build.sh
   ./build.sh
   ./anpr
   ```
