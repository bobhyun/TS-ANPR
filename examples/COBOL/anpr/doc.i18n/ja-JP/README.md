[English](../../README.md) | [한국어](../ko-KR/README.md) | 日本語 | [Tiếng Việt](../vi-VN/README.md)

# COBOL サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/COBOL/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/COBOL/anpr)

このサンプルは、Cラッパーライブラリを使用してCOBOLアプリケーションにTS-ANPRエンジンを統合する方法を示します。

## アーキテクチャ

COBOLサンプルは階層化されたアーキテクチャを使用します：

```
COBOLアプリケーション (anpr.cbl)
         ↓
Cラッパーライブラリ (libtsanpr_cobol.so/.dll)
         ↓
TS-ANPRエンジン (libtsanpr.so/tsanpr.dll)
```

- **COBOLアプリケーション**: Cラッパーを呼び出すメインプログラム
- **Cラッパー**: TS-ANPRエンジンへのCOBOL対応インターフェースを提供
- **TS-ANPRエンジン**: ディープラーニングベースのナンバープレート認識エンジン

### 1. エンジンファイルのコピー

_**【参考】** このサンプルでは、他のサンプルとエンジンファイルを共有するために `examples/bin/` ディレクトリに展開しますが、実際の配布時には通常、アプリケーションの実行ファイルがあるディレクトリにエンジンファイルをコピーします。_

- Windows x86 64ビット: `examples/bin/windows-x86_64` ディレクトリに展開
  ```sh
  tar xvf tsanpr*-windows-x86_64.tar.xz
  ```
- Windows x86 32ビット: `examples/bin/windows-x86` ディレクトリに展開
  ```sh
  tar xvf tsanpr*-windows-x86.tar.xz
  ```
- Linux x86 64ビット: `examples/bin/linux-x86_64` ディレクトリに展開
  ```sh
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- Linux arm 64ビット: `examples/bin/linux-aarch64` ディレクトリに展開
  ```sh
  tar xvf tsanpr*-linux-aarch64.tar.xz
  ```

- ディレクトリ構成
  ```
  examples
  ├── bin
  │   ├── windows-x86_64        # Windows (x86_64) エンジンディレクトリ
  │   ├── windows-x86           # Windows (x86) エンジンディレクトリ
  │   ├── linux-x86_64           # Linux (x86_64) エンジンディレクトリ
  │   └── linux-aarch64          # Linux (arm64) エンジンディレクトリ
  ├── img                        # 画像ディレクトリ
  └── COBOL
      └── anpr                   # ソースディレクトリ
         ├── src
         │   ├── c               # Cラッパーソースファイル
         │   │   ├── tsanpr_cobol.c
         │   │   └── tsanpr_cobol.h
         │   └── cobol           # COBOLソースファイル
         │       └── anpr.cbl
         ├── bin                 # ビルド出力ディレクトリ
         ├── compile.bat         # Windowsビルドスクリプト
         ├── compile.sh          # Linuxビルドスクリプト
         ├── run.bat             # Windows実行スクリプト
         ├── run.sh              # Linux実行スクリプト
         └── Makefile            # GNU Makeビルドファイル
  ```

### 2. ビルドと実行

#### 2.1 Windows

1. **GnuCOBOLのインストール**

   - [GnuCOBOL for Windows](https://sourceforge.net/projects/gnucobol/) をダウンロードしてインストール
   - または [SuperBOL](https://superbol.eu/developers/windows/) 経由でインストール（推奨）
   - GnuCOBOL binディレクトリをPATHに追加

2. **GCCのインストール**

   - Cラッパーのビルドには GCC が必要です
   - SuperBOLをインストールした場合、MinGW64 GCCが含まれています
   - それ以外の場合は、[MinGW-w64](https://www.mingw-w64.org/) をインストール

3. **ビルド方法**

   ```cmd
   compile.bat
   ```

   またはMakeを使用：

   ```cmd
   make
   ```

4. **実行方法**

   ```cmd
   run.bat
   ```

   またはMakeを使用：

   ```cmd
   make run
   ```

#### 2.2 Linux

1. **依存関係のインストール**

   - Debian / Ubuntu
     ```sh
     sudo apt-get update
     sudo apt-get install gnucobol gcc
     ```
   - Oracle Linux / RHEL / CentOS
     ```sh
     sudo yum install gnucobol gcc
     ```
   - Fedora
     ```sh
     sudo dnf install gnucobol gcc
     ```

2. **ビルド方法**

   ```sh
   chmod +x compile.sh
   ./compile.sh
   ```

   またはMakeを使用：

   ```sh
   make
   ```

3. **実行方法**

   ```sh
   chmod +x run.sh
   ./run.sh
   ```

   またはMakeを使用：

   ```sh
   make run
   ```

## COBOLプログラム (`src/cobol/anpr.cbl`)

以下の機能を実演します：
- 単一ナンバープレート認識
- 複数ナンバープレート認識 (vm)
- バイクを含む複数プレート (vmb)
- サラウンド検出 (vms)
- オブジェクト検出 (dms, dmsr)
- 関心領域（ROI）検出 (dmsri)

すべての機能は他の言語（C、Pythonなど）のサンプルと同等です。

## パフォーマンスに関する注意事項

- **Windows**: エンジン初期化は約1秒
- **WSL/Linux on Windows filesystem (`/mnt/`)**: ファイルシステム間のパフォーマンスオーバーヘッドにより、初期化に5～7秒かかる場合があります
- **Native Linux**: エンジン初期化は約1～2秒

WSLでより良いパフォーマンスを得るには、プロジェクト全体をWSLネイティブファイルシステム（例：`~/`）にコピーしてください。

## トラブルシューティング

### Linux: "libcob: error: module not found"

このエラーは、GnuCOBOLがCラッパーライブラリを見つけられない場合に発生します。実行スクリプトは `LD_PRELOAD` を使用してライブラリを強制的にロードします。

**解決方法**: バイナリを直接実行せず、常に `run.sh` または `make run` を使用してください。

### Windows: DLL not found エラー

以下を確認してください：
1. コンパイル後に `bin/tsanpr_cobol.dll` が存在するか
2. `COB_LIBRARY_PATH` を設定する `run.bat` を通して実行しているか

## ライセンス

このサンプルコードはMITライセンスの下で配布されています。