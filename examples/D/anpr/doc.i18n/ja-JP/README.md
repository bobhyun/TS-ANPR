[English](../../README.md) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# D サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/D/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/D/anpr)

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
  └── D
      └── anpr                   # ソースディレクトリ
         ├── anpr.d
         ├── dub.json
         └── tsanpr.d
  ```

### 2. 前提条件

1. D コンパイラのインストール（DMD、LDC、または GDC）

   **Windows:**

   ```sh
   # https://dlang.org/download.html から DMD をダウンロードしてインストール
   # または chocolatey を使用
   choco install dmd
   ```

   **Ubuntu / Debian:**

   ```sh
   # snap を使用してインストール
   sudo snap install dmd --classic
   sudo snap install dub --classic

   # または apt を使用してインストール
   sudo wget https://netcologne.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list
   sudo apt-get update && sudo apt-get -y --allow-unauthenticated install --reinstall d-apt-keyring
   sudo apt-get update && sudo apt-get install dmd-compiler dub
   ```

   **Oracle Linux / RedHat (RHEL) / CentOS:**

   ```sh
   # snap を使用してインストール
   sudo snap install dmd --classic
   sudo snap install dub --classic

   # または https://dlang.org/download.html から直接ダウンロードしてインストール
   curl -fsS https://dlang.org/install.sh | bash -s dmd
   source ~/dlang/dmd-*/activate
   ```

2. インストールの確認

   ```sh
   dmd --version
   ```

### 3. 実行方法

1. D サンプルディレクトリに移動

   ```sh
   cd D/anpr
   ```

2. サンプルのコンパイルと実行

   **DUB を使用（推奨）:**

   ```sh
   # ビルドと実行
   dub run

   # ビルドのみ
   dub build

   # リリースバージョンのビルド
   dub build --build=release

   # 実行
   ./bin/anpr
   ```

### 4. 注意事項

- この D 実装は他の言語サンプルと同じ機能を提供します
- D の優れた C 相互運用性を使用してネイティブ TSANPR ライブラリとインターフェースします
- D のシステムプログラミング機能と現代的な言語特性がパフォーマンスと安全性の両方を提供します
- Windows と Linux のクロスプラットフォームサポート
- D の組み込みメモリ管理とガベージコレクションが開発を簡素化します

### 5. 機能

- **ファイルベース認識**: 画像ファイルを直接処理
- **エンコード画像処理**: エンコードされた画像データの処理（JPEG、PNG など）
- **ピクセルバッファ処理**: 生ピクセルデータの処理（imageformats ライブラリを使用）
- **複数出力形式**: テキスト、JSON、YAML、XML、CSV 出力をサポート
- **複数認識モード**: 単一プレート、複数プレート、車両検出など
- **関心領域（RoI）**: 画像内の特定領域を処理
- **多国対応**: 異なるナンバープレート形式をサポート（KR、JP、VN など）

### 6. API リファレンス

#### TSAnpr クラス

**コンストラクタ:**

- `this(string libraryPath)`: ネイティブライブラリパスで初期化

**コアメソッド:**

- `string anprInitialize(string enginePath)`: ANPR エンジンを初期化
- `string anprReadFile(string imagePath, string options, string outputFormat)`: 画像ファイルを処理
- `string anprReadPixels(ubyte[] pixels, int width, int height, int channels, string options, string outputFormat)`: ピクセルデータを処理

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

- D コンパイラが正しくインストールされていることを確認
- ターゲットプラットフォームに適したコンパイラフラグを使用
- 複雑なプロジェクトには DUB パッケージマネージャーの使用を検討

**プラットフォーム固有の問題:**

- **Windows**: Visual C++ 再頒布可能パッケージがインストールされていることを確認
- **Linux**: 必要なシステムライブラリをインストールし、ライブラリの権限が正しいことを確認
