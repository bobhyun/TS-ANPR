[English](../../README.md) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# Crystal サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Crystal/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Crystal/anpr)

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
  └── Crystal
      └── anpr                   # ソースディレクトリ
         ├── anpr.cr
         └── tsanpr.cr
  ```

### 2. 前提条件

1. Crystal のインストール（バージョン 1.0.0 以降推奨）

   **Windows:**

   ```sh
   # Scoop を使用
   scoop install crystal

   # または https://crystal-lang.org/install/on_windows/ からダウンロード
   ```

   **Linux:**

   ```sh
   # Ubuntu/Debian
   curl -fsSL https://crystal-lang.org/install.sh | sudo bash

   # またはパッケージマネージャーを使用
   sudo apt-get install crystal
   ```

2. インストールの確認

   ```sh
   crystal --version
   ```

### 3. 実行方法

1. Crystal サンプルディレクトリに移動

   ```sh
   cd Crystal/anpr
   ```

2. 依存関係のインストール

   ```sh
   # 必要な shard のインストール（画像デコード用の StumpyPNG と StumpyJPEG）
   shards install
   ```

   **Windows 注意事項:** Shards はシンボリックリンクが必要です。シンボリックリンクエラーが発生した場合、2 つのオプションがあります:

   - **オプション 1（推奨）:** Windows 設定で開発者モードを有効にする
     - 設定 → プライバシーとセキュリティ → 開発者向け → 開発者モード → オン
     - 参照: https://learn.microsoft.com/ja-jp/windows/apps/get-started/enable-your-device-for-development

   - **オプション 2:** PowerShell またはコマンドプロンプトを管理者として実行
     ```powershell
     # 管理者として実行
     shards install
     ```

3. サンプルの実行

   **Shards を使用（推奨）:**

   ```sh
   # ビルドと実行
   shards build
   ./bin/anpr

   # 最適化付きでビルド
   shards build --release
   ```

   **直接コンパイル:**

   ```sh
   # 直接実行
   crystal run anpr.cr

   # またはコンパイル後実行
   crystal build anpr.cr
   ./anpr

   # 最適化付きでコンパイル
   crystal build --release anpr.cr
   ```

### 4. 注意事項

- この Crystal 実装は他の言語サンプルと同じ機能を提供します
- Crystal の`lib`バインディングを使用してネイティブ TSANPR ライブラリとインターフェースします
- Crystal の Ruby のような構文と C のようなパフォーマンスにより、システムプログラミングに理想的です
- Windows と Linux のクロスプラットフォームサポート
- ガベージコレクションとコンパイル時 null チェックによるメモリ安全性

### 5. 機能

- **ファイルベース認識**: `anpr_read_file`を使用した画像ファイルの直接処理
- **エンコード画像処理**: バイト配列としてエンコードされた画像データの処理（JPEG、PNG など）
- **ピクセルバッファ処理**: StumpyPNG および StumpyJPEG ライブラリを使用して RGB 形式にデコードされたピクセルデータの処理
- **複数出力形式**: テキスト、JSON、YAML、XML、CSV 出力をサポート
- **複数認識モード**: 単一プレート、複数プレート、車両検出など
- **関心領域（RoI）**: 画像内の特定領域を処理
- **多国対応**: 異なるナンバープレート形式をサポート（KR、JP、VN など）

### 6. 画像処理方法

このサンプルは、3 つの異なる方法で画像を処理する方法を示しています:

1. **read_image_file**: `anpr_read_file`を使用した直接ファイル処理（最速の方法）
2. **read_encoded_image**: ピクセル形式 "encoded" でエンコードされた画像バイトを渡す
3. **read_pixel_buffer**: StumpyPNG/StumpyJPEG を使用して生 RGB ピクセルデータにデコードしてから`anpr_read_pixels`に渡す
   - PNG ファイル: StumpyPNG を使用してデコード
   - JPEG ファイル: StumpyJPEG を使用してデコードし、デコード失敗時は自動的にエンコード形式にフォールバック

方法を切り替えるには、コード内の`anpr_func`変数を変更してください:

```crystal
# 次のいずれかを選択:
anpr_func = ->read_image_file(TSANPR, String, String, String)
# anpr_func = ->read_encoded_image(TSANPR, String, String, String)
# anpr_func = ->read_pixel_buffer(TSANPR, String, String, String)
```

**注意**: `read_pixel_buffer`関数は JPEG デコード失敗を自動的に処理し、エンコード形式にフォールバックすることで、さまざまな JPEG バリアントで確実に動作します。

### 7. API リファレンス

#### TSANPR クラス

`TSANPR`クラスは以下のメソッドを提供します：

**コンストラクタ:**

- `TSANPR.new(library_path : String)`: ネイティブライブラリパスで初期化

**コアメソッド:**

- `anpr_initialize(mode : String) : String`: ANPR エンジンを初期化
- `anpr_read_file(img_file_name : String, output_format : String, options : String) : String`: 画像ファイルを処理
- `anpr_read_pixels(pixels : UInt8*, width : UInt64, height : UInt64, stride : Int64, pixel_format : String, output_format : String, options : String) : String`: ピクセルデータを処理

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

**コンパイルの問題:**

- Crystal コンパイラが正しくインストールされていることを確認
- コンパイル中にライブラリパスが存在することを確認
- 最適化されたビルドには`crystal build --release`を使用

**プラットフォーム固有の問題:**

- **Windows**: Visual C++ 再頒布可能パッケージがインストールされていることを確認
- **Linux**: 必要なシステムライブラリをインストールし、ライブラリの権限が正しいことを確認
