[English](../../) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# R サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/R/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/R/anpr)

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
  └── R
      └── anpr                   # ソースディレクトリ
         ├── anpr.R              # メインサンプルスクリプト
         ├── tsanpr.R            # TSANPR R6ラッパークラス
         ├── src/                # Cラッパーソース
         │   ├── tsanpr_r.c
         │   ├── Makevars
         │   └── Makevars.win
         └── DESCRIPTION
  ```

### 2. 前提条件

1. Rとシステム依存関係のインストール

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get update
   # Rと開発ツールのインストール
   sudo apt-get install -y r-base r-base-dev
   # Rパッケージ用システムライブラリのインストール
   sudo apt-get install -y libcurl4-openssl-dev libmagick++-dev
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   # CentOS/RHEL 8+ / Fedora
   sudo dnf install -y R R-devel
   sudo dnf install -y libcurl-devel ImageMagick-c++-devel

   # CentOS/RHEL 7
   sudo yum install -y R R-devel
   sudo yum install -y libcurl-devel ImageMagick-c++-devel
   ```

   **Windows:**

   - https://cran.r-project.org/bin/windows/base/ からRをダウンロード
   - https://cran.r-project.org/bin/windows/Rtools/ からRtoolsをインストール

2. 必要なRパッケージのインストール

   ```sh
   # R6パッケージのインストール（必須）
   sudo Rscript -e 'install.packages("R6", repos="https://cloud.r-project.org")'

   # magickパッケージのインストール（オプション、readPixelBuffer用）
   sudo Rscript -e 'install.packages("magick", repos="https://cloud.r-project.org")'
   ```

   _注意: Windowsでは`sudo`なしでRscriptコマンドを実行してください。_

3. Cラッパーのコンパイル

   **Linux/macOS:**

   ```sh
   cd examples/R/anpr
   R CMD SHLIB src/tsanpr_r.c -o src/tsanpr_r.so
   ```

   **Windows:**

   ```sh
   cd examples/R/anpr
   R CMD SHLIB src/tsanpr_r.c -o src/tsanpr_r.dll
   ```

### 3. 実行方法

```sh
cd examples/R/anpr

# ANPRサンプルを実行
Rscript anpr.R
```

**Rコンソールから:**

```r
setwd("examples/R/anpr")
source("anpr.R")
```

**インタラクティブモード:**

```r
# TSANPRラッパーをロード
source("tsanpr.R")

# TSANPRを初期化
engine_path <- "../../bin/linux-x86_64/libtsanpr.so"  # プラットフォームに合わせて調整
tsanpr <- TSANPR$new(engine_path)

# エンジンを初期化
tsanpr$anpr_initialize("text;country=KR")

# 画像を処理
result <- tsanpr$anpr_read_file("../../img/KR/licensePlate.jpg", "text", "")
print(result)
```

### 4. 機能

- **readImageFile**: `anpr_read_file()`を使用して画像ファイルを直接処理
- **readEncodedImage**: 'encoded'形式で`anpr_read_pixels()`を使用してエンコードされた画像バイトを処理
- **readPixelBuffer**: magickパッケージを使用して生のRGBピクセルデータを処理
- **動的ロード**: TSANPRライブラリはCラッパーを介して実行時にロード
- **クロスプラットフォーム**: WindowsとLinux（x86_64、ARM64）をサポート

### 5. APIリファレンス

#### TSANPRクラス

`TSANPR` R6クラスは以下のメソッドを提供します：

```r
TSANPR <- R6Class("TSANPR",
  public = list(
    initialize = function(library_path),
    anpr_initialize = function(mode),
    anpr_read_file = function(img_file_name, output_format, options),
    anpr_read_pixels = function(pixels, width, height, stride, pixel_format, output_format, options),
    is_loaded = function()
  )
)
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

このサンプルでは、RのFFIとTSANPRライブラリを接続するためにCラッパー（`src/tsanpr_r.c`）を使用しています：

**動的ライブラリロード:**
- **Linux**: libdlの`dlopen()`、`dlsym()`、`dlclose()`を使用
- **Windows**: kernel32の`LoadLibrary()`、`GetProcAddress()`、`FreeLibrary()`を使用

**R統合:**
- 効率的なデータ交換のためにRの`.Call()`インターフェースを使用
- `R_registerRoutines()`を通じてCルーチンを適切に登録
- R6クラスでクリーンなオブジェクト指向インターフェースを提供

RコードはコンパイルされたCラッパーをロードし、CラッパーはTSANPRライブラリ関数を動的にロードして呼び出します。

### 7. トラブルシューティング

**コンパイルの問題:**

- R開発パッケージがインストールされていることを確認（Debian/Ubuntuでは`r-base-dev`）
- WindowsではRtoolsをインストールしてPATHに追加
- Cコンパイラが利用可能か確認: `R CMD config CC`

**ライブラリロードの問題:**

- コンパイルされたラッパーが`src/`ディレクトリにあることを確認
- TSANPRライブラリのパスが正しいことを確認
- Linuxでは必要に応じて`LD_LIBRARY_PATH`を設定

**パッケージの依存関係:**

- R6パッケージをインストール: `install.packages("R6")`
- ピクセルバッファ処理用: `install.packages("magick")`
