[English](../../README.md) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# Groovy サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Groovy/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Groovy/anpr)

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
  └── Groovy/
      └── anpr/                 # Groovyソースディレクトリ
          ├── anpr.groovy       # メインサンプル
          └── TSANPR.groovy     # TSANPR JNAラッパーモジュール
  ```

### 2. 前提条件

1. Java JDK 8 以上をインストール

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get update
   sudo apt-get install -y openjdk-11-jdk
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   sudo dnf install java-11-openjdk-devel
   ```

   **macOS:**

   ```sh
   brew install openjdk@11
   ```

   **Windows:**

   - https://adoptium.net/ または https://www.oracle.com/java/technologies/downloads/ からJDKをダウンロードしてインストール

2. Groovyをインストール

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get install -y groovy
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   sudo dnf install groovy
   ```

   **macOS:**

   ```sh
   brew install groovy
   ```

   **Windows:**

   - https://groovy.apache.org/download.html からダウンロード
   - またはSDKMANを使用: `sdk install groovy`

3. インストールの確認

   ```sh
   java -version
   groovy --version
   ```

### 3. 実行方法

```sh
cd examples/Groovy/anpr

# ANPRサンプルを実行
groovy anpr.groovy
```

### 4. 機能

- **readImageFile**: `anpr_read_file()`を使用して画像ファイルを直接処理
- **readEncodedImage**: 'encoded'形式で`anpr_read_pixels()`を使用してエンコードされた画像バイトを処理
- **readPixelBuffer**: Java AWT BufferedImageを使用して生のRGBピクセルデータを処理
- **動的ロード**: TSANPRライブラリはJNAを介して実行時にロード
- **クロスプラットフォーム**: WindowsとLinux（x86_64、x86、ARM64）をサポート
- **自動依存関係**: JNAはGrape（@Grabアノテーション）を介して自動ダウンロード

### 5. APIリファレンス

#### TSANPRモジュール

`TSANPR`クラスは以下のメソッドを提供します：

```groovy
// TSANPRライブラリをロード
def tsanpr = new TSANPR(libraryPath)

// ANPRエンジンを初期化
String error = tsanpr.initialize(mode)

// 画像ファイルを読み取り処理
String result = tsanpr.readFile(imgFileName, outputFormat, options)

// ピクセルデータを直接処理
String result = tsanpr.readPixels(pixels, width, height, stride, pixelFormat, outputFormat, options)
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
| `"dmsri<座標>"` | 関心領域内で認識 |

#### 出力形式

`"text"`, `"json"`, `"yaml"`, `"xml"`, `"csv"`

### 6. 実装に関する注意事項

このサンプルは動的ライブラリロードにJNA（Java Native Access）を使用します：

**動的ライブラリロード：**
- JNAはJNIボイラープレートなしでネイティブ共有ライブラリへの簡単なアクセスを提供
- `TSANPRLibrary`インターフェースがネイティブ関数シグネチャを定義
- `Native.load()`メソッドを使用してライブラリをロード

**Groovy機能：**
- Grape（@Grabアノテーション）を使用した自動依存関係管理
- JNAライブラリは初回実行時に自動ダウンロード
- 柔軟な関数選択のためのクロージャとメソッド参照

**ピクセルバッファ処理：**
`readPixelBuffer`関数は、画像のロードとデコードにJavaの`BufferedImage`を使用します。生のピクセルデータを抽出して、JNAの`Memory`クラスを使用して適切なピクセル形式で`anpr_read_pixels()`に渡します。

### 7. トラブルシューティング

**Groovyが見つからない：**
```sh
# Groovyのインストールを確認
groovy --version

# Ubuntu/Debian: Groovyをインストール
sudo apt-get install groovy

# またはSDKMANを使用
curl -s "https://get.sdkman.io" | bash
sdk install groovy
```

**JNA依存関係の問題：**
```sh
# GrapeがJNAを自動的にダウンロードするはず
# 問題が続く場合は、ネットワーク接続またはプロキシ設定を確認

# Grapeキャッシュをクリアして再試行
rm -rf ~/.groovy/grapes/net.java.dev.jna
groovy anpr.groovy
```

**ライブラリロードの問題：**
- TSANPRライブラリが予想される場所に存在することを確認
- Linuxでは必要に応じて`LD_LIBRARY_PATH`を確認
- ライブラリアーキテクチャがJVMと一致することを確認（64ビット vs 32ビット）

**ランタイムの問題：**
- エンジンファイル（`.eon`）がライブラリと同じディレクトリにあることを確認
- `tshelper`を使用してライセンスがインストールされていることを確認
