[English](../../) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# Zig サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Zig/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Zig/anpr)

### 1. エンジンファイルのコピー

_**[注意]** このサンプルでは、他のサンプルとエンジンファイルを共有するために examples/bin/ ディレクトリに配置します。実際の配布時には、通常はアプリケーションの実行ファイルと同じディレクトリにエンジンファイルをコピーします。_

- Windows x86 64ビット
  エンジンファイルを `examples/bin/windows-x86_64` ディレクトリに解凍
  ```sh
  tar xvf tsanpr*-windows-x86_64.tar.xz
  ```
- Windows x86 32ビット
  エンジンファイルを `examples/bin/windows-x86` ディレクトリに解凍
  ```sh
  tar xvf tsanpr*-windows-x86.tar.xz
  ```
- Linux x86 64ビット
  エンジンファイルを `examples/bin/linux-x86_64` ディレクトリに解凍
  ```sh
  tar xvf tsanpr-linux-x86_64.tar.xz
  ```
- Linux arm 64ビット
  エンジンファイルを `examples/bin/linux-aarch64` ディレクトリに解凍
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
  └── Zig
      └── anpr                   # ソースディレクトリ
         ├── build.zig
         ├── src/
         │   ├── main.zig
         │   ├── tsanpr.c
         │   ├── tsanpr.h
         │   └── tsanpr.zig
         └── zig-out/
             └── bin/            # 出力ディレクトリ
  ```

### 2. 前提条件

1. Zigのインストール (最新の安定版を推奨)

   **Windows:**

   ```sh
   # https://ziglang.org/download/ からダウンロード
   # またはScoopを使用
   scoop install zig
   ```

   **Linux:**

   ```sh
   # https://ziglang.org/download/ からダウンロード
   # またはパッケージマネージャーを使用（ディストリビューションにより異なります）
   sudo snap install zig --classic --beta
   ```

2. インストールの確認

   ```sh
   zig version
   ```

### 3. 実行方法

1. Zigサンプルディレクトリに移動

   ```sh
   cd Zig/anpr
   ```

2. サンプルのビルドと実行

   ```sh
   # ビルドと実行
   zig build run

   # ビルドのみ
   zig build

   # 最適化してビルド
   zig build -Doptimize=ReleaseFast

   # テストの実行
   zig build test
   ```

### 4. 注意事項

- Zig実装は他の言語サンプルと同じ機能を提供します
- ZigのC相互運用性を使用してネイティブTSANPRライブラリとインターフェースします
- Zigのコンパイル時安全性とパフォーマンスはシステムプログラミングに最適です
- WindowsおよびLinuxクロスプラットフォームサポート
- より良い配布柔軟性のためのランタイムライブラリロード

### 5. 機能

- **ファイルベース認識**: 画像ファイルを直接処理
- **エンコードされた画像処理**: エンコードされた画像データの処理 (JPEG、PNGなど)
- **ピクセルバッファ処理**: 生ピクセルデータの処理（簡略化された実装）
- **複数の出力形式**: テキスト、JSON、YAML、XML、CSV出力をサポート
- **複数の認識モード**: 単一プレート、複数プレート、車両検出など
- **関心領域 (RoI)**: 画像内の特定領域の処理
- **多国間サポート**: 異なるナンバープレート形式のサポート (KR、JP、VNなど)

### 6. APIリファレンス

#### TSANPR関数

Zig実装はCインターオペレーションを通じて以下の関数を提供します：

**初期化:**

- `TSANPR_load(library_path)`: TSANPRライブラリをロード
- `anpr_initialize(mode)`: ANPRエンジンを初期化

**コア関数:**

- `anpr_read_file(img_file_name, output_format, options)`: 画像ファイルを処理
- `anpr_read_pixels(pixels, width, height, stride, pixel_format, output_format, options)`: ピクセルデータを処理

#### 認識オプション

- `""`: 単一ナンバープレート認識（デフォルト）
- `"vm"`: 車両に取り付けられた複数のナンバープレートを認識
- `"vmb"`: 車両に取り付けられた複数のナンバープレートを認識（オートバイを含む）
- `"vms"`: サラウンド検出で車両の複数のナンバープレートを認識
- `"dms"`: 複数のサラウンドオブジェクト（車両）を認識
- `"dmsr"`: 複数のサラウンドオブジェクト（車両）とナンバープレートを認識
- `"dmsri<座標>"`: 関心領域内で認識

#### 出力形式

- `"text"`: プレーンテキスト出力
- `"json"`: JSON形式出力
- `"yaml"`: YAML形式出力
- `"xml"`: XML形式出力
- `"csv"`: CSV形式出力
