[English](../../README.md) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# PHP サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/PHP/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/PHP/anpr)

### 1. エンジンファイルのコピー

_**[注意]** このサンプルでは、他のサンプルと共有するためにエンジンファイルを examples/bin/ディレクトリに展開します。ただし、実際のデプロイメントでは、通常、アプリケーションの実行ファイルが配置されているディレクトリにエンジンファイルをコピーします。_

- Windows x86 64ビット用
  エンジンファイルを`examples/bin/windows-x86_64`ディレクトリに展開
  ```sh
  7z x tsanpr*-windows-x86_64.7z
  ```
- Windows x86 32ビット用
  エンジンファイルを`examples/bin/windows-x86`ディレクトリに展開
  ```sh
  7z x tsanpr*-windows-x86.7z
  ```
- Linux x86 64ビット用
  エンジンファイルを`examples/bin/linux-x86_64`ディレクトリに展開
  ```sh
  tar xvf tsanpr-linux-x86_64.tar.xz
  ```
- Linux arm 64ビット用
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
  └── PHP
      └── anpr                   # ソースディレクトリ
         ├── anpr.php
         ├── TSANPR.php
         └── composer.json
  ```

### 2. 前提条件

1. PHP 8.0以上と必要な拡張機能のインストール

   **Ubuntu/Debian:**

   ```sh
   # ondrej/php PPAを追加
   sudo apt-get update
   sudo apt-get install -y software-properties-common
   sudo add-apt-repository -y ppa:ondrej/php
   sudo apt-get update

   # PHPと必要な拡張機能をインストール
   sudo apt-get install -y php8.2 php8.2-cli php8.2-imagick

   # php.iniでFFIを有効化
   sudo sed -i 's/;ffi.enable.*/ffi.enable=true/' /etc/php/8.2/cli/php.ini
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   sudo dnf install php php-cli php-pecl-imagick

   # php.iniでFFIを有効化
   sudo sed -i 's/;ffi.enable.*/ffi.enable=true/' /etc/php.ini
   ```

   **macOS:**

   ```sh
   brew install php imagemagick
   pecl install imagick

   # php.iniでFFIを有効化
   echo "ffi.enable=true" >> $(php --ini | grep "Loaded Configuration" | sed -e "s|.*:\s*||")
   ```

   **Windows:**

   - https://windows.php.net/download/ からPHPをダウンロード
   - ImageMagickとphp_imagick.dllをインストール
   - php.iniで有効化:
     ```ini
     ffi.enable=true
     extension=imagick
     ```

2. インストールの確認

   ```sh
   php --version
   php -m | grep -E "(FFI|imagick)"
   ```

### 3. 実行方法

```sh
cd examples/PHP/anpr
php anpr.php
```

### 4. 機能

- **readImageFile**: `anpr_read_file()`を使用して画像ファイルを直接処理
- **readEncodedImage**: `anpr_read_pixels()`の'encoded'形式でエンコードされた画像データ（JPEG、PNGなど）を処理
- **readPixelBuffer**: Imagick拡張を使用して生ピクセルデータを処理

### 5. APIリファレンス

#### TSANPRクラス

```php
class TSANPR {
    public function __construct(string $libraryPath);
    public function anprInitialize(string $mode): string;
    public function anprReadFile(string $imgFileName, string $outputFormat, string $options): string;
    public function anprReadPixels(string $pixels, int $width, int $height, int $stride,
                                   string $pixelFormat, string $outputFormat, string $options): string;
}
```

#### 認識オプション

| オプション | 説明 |
|------------|------|
| `""` | 単一ナンバープレート認識（デフォルト） |
| `"vm"` | 車両に取り付けられた複数ナンバープレートを認識 |
| `"vmb"` | 複数ナンバープレートを認識（オートバイを含む） |
| `"vms"` | 周辺検出付きで認識 |
| `"dms"` | 複数の周辺オブジェクト（車両）を検出 |
| `"dmsr"` | オブジェクト検出とナンバープレート認識 |
| `"dmsri<座標>"` | 関心領域内で認識 |

#### 出力形式

`"text"`, `"json"`, `"yaml"`, `"xml"`, `"csv"`

### 6. トラブルシューティング

**FFIがロードされない:**
```sh
# FFIステータスを確認
php -i | grep -i ffi

# php.iniでFFIを有効化
ffi.enable=true
```

**Imagickが利用できない:**
```sh
# Ubuntu/Debian
sudo apt-get install php8.2-imagick

# 確認
php -m | grep imagick
```
