[English](../../README.md) | [한국어](../ko-KR/README.md) | 日本語 | [Tiếng Việt](../vi-VN/README.md)

# Pascal サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Pascal/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Pascal/anpr)

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
  │   ├─── windows-x86_64        # Windows (x86_64) エンジンディレクトリ
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86           # Windows (x86) エンジンディレクトリ
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64           # Linux (x86_64) エンジンディレクトリ
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # Linux (arm64) エンジンディレクトリ
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # 画像ディレクトリ
  └── Pascal
      └── anpr                   # プロジェクトディレクトリ
         ├── src                 # メインソースディレクトリ
         │   └── anpr.pas        # メインプログラム
         ├── units               # ユニットディレクトリ
         │   ├── tsanpr.pas      # TSANPRユニット
         │   └── stb_image.pas   # stb_imageバインディング
         ├── lib                 # 外部ライブラリソース
         │   ├── stb_image.h     # stb_imageヘッダー
         │   └── stb_image_lib.c # stb_image共有ライブラリソース
         ├── bin                 # 出力ディレクトリ（ビルド生成）
         ├── compile.bat
         ├── compile.sh
         └── Makefile
  ```

### 2. ビルドと実行

#### 2.1 Windows

1. Free Pascal のインストール

   - [Free Pascal](https://www.freepascal.org/download.html) をダウンロードしてインストール
   - PATH に Free Pascal bin ディレクトリを追加

2. ビルド方法

   ```cmd
   compile.bat
   ```

3. 実行方法

   ```cmd
   cd bin
   anpr.exe
   ```

#### 2.2 Linux

1. 依存関係のインストール

   - Debian / Ubuntu Linux

     ```sh
     sudo apt-get update
     sudo apt-get install fpc
     ```

   - Oracle Linux / RedHat (RHEL) / CentOS

     ```sh
     sudo yum install fpc
     ```

   - Fedora

     ```sh
     sudo dnf install fpc
     ```

2. ビルド方法

   ```sh
   chmod +x compile.sh
   ./compile.sh
   ```

   または Make を使用:

   ```sh
   make
   ```

3. 実行方法

   ```sh
   cd bin
   ./anpr
   ```
