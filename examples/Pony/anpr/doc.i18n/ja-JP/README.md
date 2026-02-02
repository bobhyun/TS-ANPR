[English](../../) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# Pony サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Pony/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Pony/anpr)

### 1. エンジンファイルのコピー

_**[注意]** このサンプルでは、他のサンプルと共有するためにエンジンファイルをexamples/bin/ディレクトリに展開します。ただし、実際のデプロイでは、通常、アプリケーションの実行ファイルがあるディレクトリにエンジンファイルをコピーします。_

- Windows x86 64ビットの場合
  エンジンファイルを`examples/bin/windows-x86_64`ディレクトリに展開します
  ```sh
  7z x tsanpr*-windows-x86_64.7z
  ```
- Windows x86 32ビットの場合
  エンジンファイルを`examples/bin/windows-x86`ディレクトリに展開します
  ```sh
  7z x tsanpr*-windows-x86.7z
  ```
- Linux x86 64ビットの場合
  エンジンファイルを`examples/bin/linux-x86_64`ディレクトリに展開します
  ```sh
  tar xvf tsanpr-linux-x86_64.tar.xz
  ```
- Linux arm 64ビットの場合
  エンジンファイルを`examples/bin/linux-aarch64`ディレクトリに展開します
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
  └── Pony
      └── anpr                   # プロジェクトディレクトリ
         ├── src/                # メインパッケージ
         │   ├── main.pony
         │   └── tsanpr.pony
         ├── ffi/                # C FFIラッパー
         │   ├── tsanpr_wrapper.c
         │   ├── tsanpr_wrapper.h
         │   ├── image_loader.c
         │   └── stb_image.h
         └── Makefile
  ```

### 2. 前提条件

1. Ponyコンパイラのインストール

   **Ubuntu/Debian:**

   ```sh
   # ponyupを使用（推奨）
   curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/ponylang/ponyup/latest-release/ponyup-init.sh | sh
   source ~/.profile
   ponyup update ponyc release
   ```

   **macOS:**

   ```sh
   brew install ponyc
   ```

   **Windows:**

   ```sh
   # Chocolateyを使用
   choco install ponyc

   # または https://github.com/ponylang/ponyc/releases からダウンロード
   ```

2. Cコンパイラのインストール（GCCまたはClang）

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get install build-essential
   ```

   **macOS:**

   ```sh
   xcode-select --install
   ```

   **Windows:**

   - Visual Studio Build ToolsまたはMinGW-w64をインストール

3. インストールの確認

   ```sh
   ponyc --version
   gcc --version
   ```

### 3. 実行方法

```sh
cd examples/Pony/anpr

# ビルドして実行
make run

# ビルドのみ
make

# リリースバージョンをビルド
make release

# ビルド成果物をクリーン
make clean
```

### 4. 機能

- **readImageFile**: `anpr_read_file()`を使用して画像ファイルを直接処理
- **readEncodedImage**: 'encoded'形式で`anpr_read_pixels()`を使用してエンコードされた画像バイトを処理
- **readPixelBuffer**: stb_imageライブラリを使用して生のRGBピクセルデータを処理
- **動的ロード**: TSANPRライブラリはdlopen/LoadLibraryを使用して実行時にロード
- **クロスプラットフォーム**: WindowsとLinux（x86_64、ARM64）をサポート

### 5. APIリファレンス

#### TSANPRクラス

```pony
class TSANPR
  new create(library_path: String) ?
  fun ref initialize(mode: String): String
  fun ref read_file(img_file_name: String, output_format: String, options: String): String
  fun ref read_pixels(pixels: Array[U8] val, width: U64, height: U64, stride: I64,
                     pixel_format: String, output_format: String, options: String): String
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

このサンプルでは、動的ライブラリのロードと画像処理のためにCラッパーを使用しています：

**動的ライブラリロード（`tsanpr_wrapper.c`）:**
- **Linux**: libdlの`dlopen()`、`dlsym()`、`dlclose()`を使用
- **Windows**: kernel32の`LoadLibrary()`、`GetProcAddress()`、`FreeLibrary()`を使用

**画像処理（`image_loader.c`）:**
- 画像デコードのために[stb_image](https://github.com/nothings/stb)シングルヘッダーライブラリを使用
- 画像をロードしてANPRを直接呼び出す便利関数`image_anpr_read()`を提供

PonyコードはFFIを通じてCラッパー関数を呼び出し、Cラッパー関数は動的にロードされたTSANPRライブラリ関数を呼び出します。
