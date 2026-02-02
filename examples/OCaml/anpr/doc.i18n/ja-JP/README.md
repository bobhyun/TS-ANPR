[English](../../README.md) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# OCaml サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/OCaml/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/OCaml/anpr)

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
  └── OCaml
      └── anpr                   # プロジェクトディレクトリ
          ├── bin                # 実行ファイルディレクトリ
          │   ├── dune
          │   └── main.ml
          ├── lib                # ライブラリディレクトリ
          │   ├── dune
          │   ├── tsanpr.ml
          │   ├── stb_image.h
          │   └── stb_image_stubs.c
          └── dune-project
  ```

### 2. 実行方法

1. OCaml と opam のインストール

   ```sh
   # Linux (Ubuntu/Debian)
   sudo apt-get install ocaml opam

   # opam を初期化
   opam init
   eval $(opam env)
   ```

2. 依存関係のインストール

   ```sh
   opam install dune ctypes ctypes-foreign
   ```

3. `anpr` を実行

   ```sh
   cd examples/OCaml/anpr
   dune exec anpr
   ```
