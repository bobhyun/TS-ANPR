[English](../../) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# V サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/V/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/V/anpr)

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
  ```
  examples/
  ├── bin/
  │   ├── windows-x86_64/       # Windows (x86_64)用エンジン
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── windows-x86/          # Windows (x86)用エンジン
  │   │   └── ...
  │   ├── linux-x86_64/         # Linux (x86_64)用エンジン
  │   │   └── ...
  │   └── linux-aarch64/        # Linux (arm64)用エンジン
  │       └── ...
  ├── img/
  └── V/
      └── anpr/                 # Vモジュールディレクトリ
          ├── v.mod             # モジュール定義
          ├── anpr.v            # メインサンプル
          └── tsanpr.v          # TSANPRラッパーモジュール
  ```

### 2. 前提条件

1. V のインストール（最新版推奨）

   **Windows:**

   ```sh
   # Scoop を使用
   scoop install vlang

   # またはビルド済みバイナリをダウンロード:
   # https://github.com/vlang/v/releases
   ```

   **Linux:**

   ```sh
   # ソースからクローンしてビルド
   git clone https://github.com/vlang/v
   cd v
   make
   sudo ./v symlink
   ```

2. インストールの確認

   ```sh
   v version
   ```

### 3. 実行方法

```sh
cd examples/V/anpr

# ANPRサンプルを実行
v run .
```

**その他の方法:**

```sh
# コンパイルして別々に実行
v .
./anpr        # Linux
anpr.exe      # Windows

# 最適化付きでコンパイル
v -prod .
```

### 4. 機能

- **readImageFile**: `anpr_read_file()`を使用して画像ファイルを直接処理
- **readEncodedImage**: 'encoded'形式で`anpr_read_pixels()`を使用してエンコードされた画像バイトを処理
- **readPixelBuffer**: Vの`stbi`モジュール（stb_image）を使用して生のRGBピクセルデータを処理
- **動的ロード**: TSANPRライブラリはVの`dl`モジュールを介して実行時にロード
- **クロスプラットフォーム**: WindowsとLinux（x86_64、x86、ARM64）をサポート

### 5. APIリファレンス

#### TSANPRモジュール

`tsanpr`モジュールは以下を提供します：

```v
// TSANPRインスタンスを作成
pub fn new(library_path string) !TSANPR

// TSANPRメソッド
pub fn (mut t TSANPR) destroy()
pub fn (t &TSANPR) initialize(mode string) string
pub fn (t &TSANPR) read_file(img_file_name string, output_format string, options string) string
pub fn (t &TSANPR) read_pixels(pixels []u8, width u64, height u64, stride i64, pixel_format string, output_format string, options string) string
```

#### 認識オプション

| オプション | 説明 |
|------------|------|
| `""` | 単一ナンバープレート認識（デフォルト） |
| `"vm"` | 車両に取り付けられた複数のナンバープレートを認識 |
| `"vmb"` | 複数のナンバープレートを認識（バイクを含む） |
| `"vms"` | サラウンド検出付きで認識 |
| `"dms"` | 複数の周囲オブジェクト（車両）を検出 |
| `"dmsr"` | オブジェクトを検出してナンバープレートを認識 |
| `"dmsri<coords>"` | 関心領域内で認識 |

#### 出力形式

`"text"`, `"json"`, `"yaml"`, `"xml"`, `"csv"`

### 6. 実装に関する注意事項

このサンプルは動的ライブラリロードにVの`dl`モジュールを使用します：

**動的ライブラリロード:**
- **Linux**: Vの`dl`モジュールを通じて`dlopen()`、`dlsym()`、`dlclose()`を使用
- **Windows**: Vの`dl`モジュールを通じて`LoadLibrary()`、`GetProcAddress()`、`FreeLibrary()`を使用

**V統合:**
- VのCインターオップによりネイティブライブラリへの直接関数ポインタ呼び出しが可能
- V文字列とC文字列間の自動変換
- `defer`を使用したメモリ安全なクリーンアップ

**ピクセルバッファ処理:**
`readPixelBuffer`関数は、画像デコードにVの組み込み`stbi`モジュール（stb_imageラッパー）を使用します。画像をロードし、生のRGBピクセルデータを抽出して、適切なピクセル形式で`anpr_read_pixels()`に渡します。

### 7. トラブルシューティング

**コンパイルの問題:**

- Vが正しくインストールされてPATHにあることを確認
- Vのバージョン確認: `v version`
- 必要に応じてVをアップデート: `v up`

**ライブラリロードの問題:**

- TSANPRライブラリが予想される場所に存在することを確認
- Linuxでは必要に応じて`LD_LIBRARY_PATH`を確認
- ライブラリアーキテクチャがVビルドと一致することを確認（64ビット vs 32ビット）

**ランタイムの問題:**

- エンジンファイル（`.eon`）がライブラリと同じディレクトリにあることを確認
- `tshelper`を使用してライセンスがインストールされていることを確認
