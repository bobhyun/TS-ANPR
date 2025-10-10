[English](../../README.md) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# Fortran サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Fortran/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Fortran/anpr)

### 1. エンジンファイルのコピー

_**[注意]** このサンプルでは、他のサンプルと共有するためにエンジンファイルを examples/bin/ディレクトリに展開します。ただし、実際のデプロイメントでは、通常、アプリケーションの実行ファイルが配置されているディレクトリにエンジンファイルをコピーします。_

- Windows x86 64 ビット用
  エンジンファイルを`examples/bin/windows-x86_64`ディレクトリに展開
  ```sh
  tar xvf tsanpr*-windows-x86_64.tar.xz
  ```
- Windows x86 32 ビット用
  エンジンファイルを`examples/bin/windows-x86`ディレクトリに展開
  ```sh
  tar xvf tsanpr*-windows-x86.tar.xz
  ```
- Linux x86 64 ビット用
  エンジンファイルを`examples/bin/linux-x86_64`ディレクトリに展開
  ```sh
  tar xvf tsanpr-linux-x86_64.tar.xz
  ```
- Linux arm 64 ビット用
  エンジンファイルを`examples/bin/linux-aarch64`ディレクトリに展開
  ```sh
  tar xvf tsanpr-linux-aarch64.tar.xz
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
  └── Fortran
      └── anpr                   # プロジェクトディレクトリ
         ├── src                 # ソースディレクトリ
         │   ├── anpr.f90
         │   └── tsanpr_module.f90
         ├── build               # ビルド出力ディレクトリ（makeによって作成）
         └── Makefile
  ```

### 2. 前提条件

1. Fortran コンパイラとビルドツールのインストール

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get update
   sudo apt-get install gfortran build-essential
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   # CentOS/RHEL 7
   sudo yum install gcc-gfortran make

   # CentOS/RHEL 8+ / Fedora
   sudo dnf install gcc-gfortran make
   ```

   **Windows (MinGW/MSYS2):**

   ```sh
   # 最初にMSYS2をインストールしてから:
   pacman -S mingw-w64-x86_64-gcc-fortran mingw-w64-x86_64-make
   ```

2. インストールの確認

   ```sh
   gfortran --version
   make --version
   ```

### 3. ビルド方法

1. Fortran サンプルディレクトリに移動

   ```sh
   cd Fortran/anpr
   ```

2. サンプルをビルド

   ```sh
   make all
   ```

3. ビルド成果物をクリーンアップ（必要に応じて）

   ```sh
   make clean
   ```

### 4. 実行方法

1. `anpr`サンプルの実行

   ```sh
   ./build/anpr
   ```

   またはWindowsで:

   ```sh
   build/anpr.exe
   ```

### 5. 注意事項

- この Fortran 実装は他の言語サンプルと同じ機能を提供します
- C 相互運用性のために現代的な Fortran 2008 標準と ISO C バインディングを使用します
- 動的ライブラリの読み込みはシステム固有の API を通じて処理されます（Unix の dlopen、Windows の LoadLibrary）

- ピクセルバッファ処理はこのサンプルで簡略化されています - 完全な実装には画像処理ライブラリとの統合が必要です
- プリプロセッサディレクティブを通じてクロスプラットフォームコンパイルをサポートします

### 6. 機能

- **ファイルベース認識**: 画像ファイルを直接処理
- **エンコード画像処理**: エンコードされた画像データの処理（JPEG、PNG など）
- **ピクセルバッファ処理**: 生ピクセルデータの処理（簡略化された実装）
- **複数出力形式**: テキスト、JSON、YAML、XML、CSV 出力をサポート
- **複数認識モード**: 単一プレート、複数プレート、車両検出など
- **関心領域（RoI）**: 画像内の特定領域を処理
- **多国対応**: 異なるナンバープレート形式をサポート（KR、JP、VN など）
- **クロスプラットフォーム互換性**: Windows、Linux サポート

### 7. API リファレンス

#### TSANPR モジュール

`tsanpr_module`は以下のタイプとプロシージャを提供します:

**タイプ:**

- `tsanpr_handle`: TSANPR ライブラリインスタンスのハンドル

**初期化:**

- `tsanpr_init(tsanpr, library_path, status)`: ライブラリパスで TSANPR を初期化
- `tsanpr_cleanup(tsanpr)`: TSANPR リソースをクリーンアップ

**コア関数:**

- `tsanpr_initialize(tsanpr, mode, error_msg, status)`: ANPR エンジンを初期化
- `tsanpr_read_file(tsanpr, img_file_name, output_format, options, result, status)`: 画像ファイルを処理
- `tsanpr_read_pixels(tsanpr, pixels, width, height, stride, pixel_format, output_format, options, result, status)`: ピクセルデータを処理

#### 認識オプション

- `""`: 単一ナンバープレート認識（デフォルト）
- `"vm"`: 車両に取り付けられた複数ナンバープレートを認識
- `"vmb"`: 車両に取り付けられた複数ナンバープレートを認識（オートバイを含む）
- `"vms"`: 周辺検出付きで車両に取り付けられた複数ナンバープレートを認識
- `"dms"`: 複数の周辺オブジェクト（車両）を認識
- `"dmsr"`: 複数の周辺オブジェクト（車両）とナンバープレートを認識
- `"dmsri<座標>"`: 関心領域内で認識

#### 出力形式

- `"text"`: プレーンテキスト出力
- `"json"`: JSON 形式出力
- `"yaml"`: YAML 形式出力
- `"xml"`: XML 形式出力
- `"csv"`: CSV 形式出力
