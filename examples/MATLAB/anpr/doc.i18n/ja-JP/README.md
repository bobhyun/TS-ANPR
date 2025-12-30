[English](../../README.md) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# MATLAB/Octave サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/MATLAB/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/MATLAB/anpr)

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
  └── MATLAB
      └── anpr                   # プロジェクトディレクトリ
         ├── src                 # ソースディレクトリ
         │   ├── anpr.m          # メインサンプルスクリプト
         │   ├── TSANPR.m        # TSANPR ラッパークラス
         │   └── mex             # MEX ソースディレクトリ
         │       ├── tsanpr_mex.c    # MEX ソースファイル
         │       └── build_mex.m     # MEX ビルドスクリプト
         └── doc.i18n            # 翻訳ドキュメント
  ```

### 2. 前提条件

#### オプション A: MATLAB（商用）

1. MATLAB のインストール（R2018b 以上推奨）

   **Windows:**
   - https://www.mathworks.com/downloads/ から MATLAB をダウンロードしてインストール
   - 有効なライセンスが必要

   **Linux:**
   - https://www.mathworks.com/downloads/ から MATLAB をダウンロードしてインストール
   - 有効なライセンスが必要

2. C コンパイラの設定
   ```matlab
   mex -setup
   ```

3. インストールの確認
   ```matlab
   version
   mex -setup
   ```

#### オプション B: GNU Octave（無料、オープンソース）

GNU Octave は MATLAB とほぼ互換性のある無料の代替ソフトウェアです。

**Windows:**

```cmd
# winget を使用
winget install GNU.Octave

# または https://octave.org/download からダウンロード
```

**Linux (Ubuntu/Debian):**

```sh
sudo apt-get update
sudo apt-get install -y octave octave-image liboctave-dev
```

**Linux (Fedora/RHEL):**

```sh
sudo dnf install -y octave octave-image octave-devel
```

インストールの確認:
```sh
octave --version
```

### 3. MEX ビルド方法

MEX ファイルは初回実行時に自動的にビルドされます。手動でビルドするには:

#### MATLAB を使用

```matlab
cd examples/MATLAB/anpr/src/mex
build_mex
```

#### GNU Octave を使用

```sh
cd examples/MATLAB/anpr/src/mex
octave --eval "build_mex"
```

### 4. 実行方法

#### MATLAB を使用

1. ソースディレクトリに移動
   ```sh
   cd examples/MATLAB/anpr/src
   ```

2. MATLAB を起動してサンプルを実行
   ```matlab
   % メイン ANPR サンプルを実行
   anpr
   ```

#### GNU Octave を使用

**Windows:**

```cmd
cd examples\MATLAB\anpr\src
octave --eval "anpr"
```

**Linux:**

```sh
cd examples/MATLAB/anpr/src
octave --eval "anpr"
```

または対話モードで実行:
```sh
octave
```
```octave
cd examples/MATLAB/anpr/src
anpr
```

### 5. 対話的な使用

```matlab
% パスを追加
addpath('mex');

% TSANPR を初期化
engine_path = '../../../bin/windows-x86_64/tsanpr.dll';  % Windows
% engine_path = '../../../bin/linux-x86_64/libtsanpr.so';  % Linux

tsanpr = TSANPR(engine_path);

% エンジンを初期化
error_msg = tsanpr.anpr_initialize('text;country=KR');
if ~isempty(error_msg)
    fprintf('エラー: %s\n', error_msg);
end

% 画像を処理
result = tsanpr.anpr_read_file('../../../img/KR/licensePlate.jpg', 'json', '');
fprintf('結果: %s\n', result);
```

### 6. 注意事項

- この実装はネイティブライブラリ統合のために MEX ファイルを使用します
- MEX ファイルは MATLAB と Octave の両方で一貫したインターフェースを提供します
- MEX ファイルがない場合、初回実行時に自動的にビルドされます
- Windows と Linux のクロスプラットフォームサポート

### 7. 機能

- **ファイルベース認識**: 画像ファイルを直接処理
- **エンコード画像処理**: エンコードされた画像データの処理（JPEG、PNG など）
- **ピクセルバッファ処理**: 画像処理関数を使用した生ピクセルデータの処理
- **複数出力形式**: テキスト、JSON、YAML、XML、CSV 出力をサポート
- **複数認識モード**: 単一プレート、複数プレート、車両検出など
- **関心領域（RoI）**: 画像内の特定領域を処理
- **多国対応**: 異なるナンバープレート形式をサポート（KR、JP、VN など）

### 8. API リファレンス

#### TSANPR クラス

**コンストラクタ:**
- `TSANPR(library_path)`: ネイティブライブラリパスで初期化

**コアメソッド:**
- `anpr_initialize(mode)`: ANPR エンジンを初期化
- `anpr_read_file(img_file_name, output_format, options)`: 画像ファイルを処理
- `anpr_read_pixels(pixels, width, height, stride, pixel_format, output_format, options)`: ピクセルデータを処理

**静的メソッド:**
- `TSANPR.isOctave()`: GNU Octave で実行中かどうかを確認

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

### 9. トラブルシューティング

**MEX ビルドの問題:**
- C コンパイラがインストールされていることを確認
  - **MATLAB**: `mex -setup` を実行して設定
  - **Octave Windows**: MinGW は Octave インストールに含まれています
  - **Octave Linux**: `liboctave-dev` または `octave-devel` をインストール
- コンパイラの利用可能性を確認: `mex -setup` (MATLAB) または `mkoctfile --version` (Octave)

**ライブラリ読み込みの問題:**
- TSANPR ライブラリパスが正しいことを確認
- すべてのシステム依存関係がインストールされていることを確認
- Linux では `LD_LIBRARY_PATH` にエンジンディレクトリが含まれていることを確認

**プラットフォーム固有の問題:**
- **Windows**: Visual C++ 再頒布可能パッケージがインストールされていることを確認
- **Linux**: 必要なシステムライブラリをインストールし、ライブラリの権限が正しいことを確認
