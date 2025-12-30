[English](../../README.md) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# Gleam サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Gleam/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Gleam/anpr)

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
  │   │   ├── tsanpr-*.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86           # Windows (x86)用エンジンディレクトリ
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-*.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64           # Linux (x86_64)用エンジンディレクトリ
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-*.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # Linux (arm64)用エンジンディレクトリ
  │       ├── libtsanpr.so
  │       ├── tsanpr-*.eon
  │       └── tshelper
  ├── img                        # 画像ディレクトリ
  └── Gleam
      └── anpr                   # プロジェクトディレクトリ
         ├── c_src               # C NIF ソースファイル
         │   ├── tsanpr_nif.c    # NIF 実装
         │   └── stb_image.h     # 画像デコードライブラリ
         ├── src                 # Gleam/Erlang ソースファイル
         │   ├── anpr.gleam      # メインモジュール
         │   ├── tsanpr.gleam    # TSANPR API モジュール
         │   ├── tsanpr_ffi.erl  # NIF ラッパーモジュール
         │   └── anpr_ffi.erl    # ヘルパー関数
         ├── priv                # コンパイル済み NIF ライブラリ（生成）
         │   └── tsanpr_nif.dll/.so
         ├── gleam.toml          # Gleam プロジェクト設定
         ├── manifest.toml       # Gleam 依存関係マニフェスト
         ├── build_nif.bat       # Windows NIF ビルドスクリプト
         ├── build_nif.sh        # Linux NIF ビルドスクリプト
         ├── Makefile            # Linux Makefile
         ├── Makefile.win        # Windows Makefile
         └── build               # Gleam ビルド出力（生成）
            └── dev/erlang/anpr
               ├── ebin          # コンパイル済み .beam ファイル
               └── priv          # ランタイム用 NIF ライブラリ
  ```

### 2. 前提条件

1. Gleam と Erlang のインストール

   **Windows:**

   ```sh
   # 最初に Erlang をインストール
   # https://www.erlang.org/downloads からダウンロード

   # Gleam をインストール
   # https://gleam.run/getting-started/installing/ からダウンロード
   ```

   **Linux:**

   ```sh
   # Erlang をインストール
   sudo apt-get install erlang

   # Gleam をインストール (x86_64)
   cd /tmp
   wget https://github.com/gleam-lang/gleam/releases/download/v1.14.0/gleam-v1.14.0-x86_64-unknown-linux-musl.tar.gz
   tar xzf gleam-v1.14.0-x86_64-unknown-linux-musl.tar.gz
   sudo mv gleam /usr/local/bin/

   # ARM64 (aarch64) 用
   wget https://github.com/gleam-lang/gleam/releases/download/v1.14.0/gleam-v1.14.0-aarch64-unknown-linux-musl.tar.gz
   tar xzf gleam-v1.14.0-aarch64-unknown-linux-musl.tar.gz
   sudo mv gleam /usr/local/bin/
   ```

2. インストールの確認

   ```sh
   gleam --version
   erl -version
   ```

### 3. 実行方法

1. Gleam サンプルディレクトリに移動

   ```sh
   cd Gleam/anpr
   ```

2. サンプルの実行

   ```sh
   # 依存関係をインストール
   gleam deps download

   # サンプルを実行
   gleam run

   # またはビルド後実行
   gleam build
   gleam run
   ```

### 4. 注意事項

- この Gleam 実装は他の言語サンプルと同じ機能を提供します
- Gleam の Erlang 相互運用性を使用してネイティブ TSANPR ライブラリとインターフェースします
- Gleam の型安全性と関数型プログラミングパラダイムが信頼性を提供します
- 優れた並行性のために Erlang 仮想マシン（BEAM）上で実行されます
- 注意：このサンプルはデモンストレーション用の簡略化されたネイティブライブラリ統合を使用しています

### 5. 機能

- **ファイルベース認識**: 画像ファイルを直接処理
- **エンコード画像処理**: エンコードされた画像データの処理（JPEG、PNG など）
- **ピクセルバッファ処理**: 生ピクセルデータの処理（簡略化された実装）
- **複数出力形式**: テキスト、JSON、YAML、XML、CSV 出力をサポート
- **複数認識モード**: 単一プレート、複数プレート、車両検出など
- **関心領域（RoI）**: 画像内の特定領域を処理
- **多国対応**: 異なるナンバープレート形式をサポート（KR、JP、VN など）

### 6. API リファレンス

#### TSANPR モジュール

Gleam 実装は以下の関数を提供します：

**初期化:**

- `tsanpr.new(library_path: String) -> Result(TSANPR, String)`: TSANPR インスタンスを作成

**コア関数:**

- `tsanpr.initialize(tsanpr: TSANPR, mode: String) -> Result(String, String)`: ANPR エンジンを初期化
- `tsanpr.read_file(tsanpr: TSANPR, img_file_name: String, output_format: String, options: String) -> Result(String, String)`: 画像ファイルを処理
- `tsanpr.read_pixels(tsanpr: TSANPR, pixels: List(Int), width: Int, height: Int, stride: Int, pixel_format: String, output_format: String, options: String) -> Result(String, String)`: ピクセルデータを処理

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

### 7. トラブルシューティング

**ライブラリ読み込みの問題:**

- TSANPR ライブラリパスが正しいことを確認
- すべてのシステム依存関係がインストールされていることを確認
- ライブラリの権限が正しいことを確認

**ビルドの問題:**

- Gleam と Erlang が正しくインストールされていることを確認
- 依存関係をインストールするために`gleam deps download`を実行
- プロジェクト構造を確認するために`gleam check`を使用

**プラットフォーム固有の問題:**

- **Windows**: Visual C++ 再頒布可能パッケージがインストールされていることを確認
- **Linux**: 必要なシステムライブラリをインストールし、ライブラリの権限が正しいことを確認
- **Erlang VM**: Erlang/OTP が正しくインストールされ設定されていることを確認
