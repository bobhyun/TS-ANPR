[English](../../README.md) | [한국어](../ko-KR/README.md) | 日本語 | [Tiếng Việt](../vi-VN/README.md)

# Ada サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Ada/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Ada/anpr)

### 1. エンジンファイルのコピー

_**【参考】** この例では、他のサンプルとエンジンファイルを共有するために `examples/bin/` ディレクトリに展開しますが、実際の配布時には通常、アプリケーションの実行ファイルがあるディレクトリにエンジンファイルをコピーします。_

- Windows x86 64 ビット
  エンジンファイルを `examples/bin/windows-x86_64` ディレクトリに展開
  ```sh
  7z x tsanpr*-windows-x86_64.7z
  ```
- Windows x86 32 ビット
  エンジンファイルを `examples/bin/windows-x86` ディレクトリに展開
  ```sh
  7z x tsanpr*-windows-x86.7z
  ```
- Linux x86 64 ビット
  エンジンファイルを `examples/bin/linux-x86_64` ディレクトリに展開
  ```sh
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- Linux ARM 64 ビット
  エンジンファイルを `examples/bin/linux-aarch64` ディレクトリに展開
  ```sh
  tar xvf tsanpr*-linux-aarch64.tar.xz
  ```
- ディレクトリ構成
  ```sh
  examples
  ├── bin
  │   ├─── windows-x86_64        # Windows (x86_64) エンジンディレクトリ
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86           # Windows (x86) エンジンディレクトリ
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64           # Linux (x86_64) エンジンディレクトリ
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # Linux (arm64) エンジンディレクトリ
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # 画像ディレクトリ
  └── Ada
      └── anpr                   # プロジェクトディレクトリ
         ├── bin                 # 実行ファイルディレクトリ
         ├── obj                 # オブジェクトファイルディレクトリ
         ├── src                 # ソースディレクトリ
         │   ├── anpr.adb
         │   ├── tsanpr.ads
         │   ├── tsanpr-windows.adb
         │   └── tsanpr-unix.adb
         ├── anpr.gpr
         ├── alire.toml
         ├── compile.bat
         ├── compile.sh
         └── Makefile
  ```

### 2. ビルドと実行

#### 2.1 Alire を使用 (推奨)

[Alire](https://alire.ada.dev/)は Ada の最新のパッケージマネージャで、ツールチェーンと依存関係を自動的に管理します。

1. Alire のインストール

   **Windows:**

   - [https://alire.ada.dev](https://alire.ada.dev)からダウンロード
   - 展開後、`alr` を PATH に追加

   **Linux:**

   **推奨: 手動インストール (全ディストリビューション)**

   [Alire Releases](https://github.com/alire-project/alire/releases)から最新バージョンをダウンロード:

   ```sh
   # 最新リリースをダウンロード (現在のバージョンはリリースページで確認)
   wget https://github.com/alire-project/alire/releases/download/v2.1.0/alr-2.1.0-bin-x86_64-linux.zip
   unzip alr-2.1.0-bin-x86_64-linux.zip
   sudo mv bin/alr /usr/local/bin/
   ```

   または [https://alire.ada.dev](https://alire.ada.dev)からダウンロード後、PATH に追加

   **代替: パッケージマネージャを使用 (古いバージョンの可能性あり)**

   - Debian/Ubuntu:
     ```sh
     sudo apt-get update
     sudo apt-get install alire
     ```
   - Fedora/RHEL/CentOS:
     ```sh
     sudo dnf install alire
     ```

   **注意:** パッケージマネージャでインストールしたバージョンで `Unexpected property count: 0` エラーが発生する場合は、上記の手動インストール方法で最新バージョンをインストールしてください。

2. ビルド

   ```sh
   alr build
   ```

   初回ビルド時、Alire は自動的に GNAT コンパイラと GPRbuild をダウンロードします。メッセージが表示されたら Enter を押してください。

3. 実行

   ```sh
   alr run
   ```

   または直接実行:

   ```sh
   bin/anpr
   ```

#### 2.2 GNAT/GPRbuild を使用 (従来の方法)

##### 2.2.1 Windows

1. GNAT のインストール

   - [GNAT Community](https://www.adacore.com/download) をダウンロードしてインストール
   - PATH に GNAT bin ディレクトリを追加

2. ビルド方法

   ```cmd
   compile.bat
   ```

   または GPRbuild を使用:

   ```cmd
   gprbuild -p -P anpr.gpr -XOS=Windows_NT
   ```

3. 実行方法

   UTF-8 文字表示のため (日本語などの non-ASCII 文字に推奨):

   ```cmd
   chcp 65001
   bin\anpr.exe
   ```

   または UTF-8 エンコーディングなし:

   ```cmd
   bin\anpr.exe
   ```

##### 2.2.2 Linux

1. 依存関係のインストール

   - Debian / Ubuntu Linux

     ```sh
     sudo apt-get update
     sudo apt-get install gnat gprbuild
     ```

   - Oracle Linux / RedHat (RHEL) / CentOS

     ```sh
     sudo yum install gcc-gnat gprbuild
     ```

   - Fedora

     ```sh
     sudo dnf install gcc-gnat gprbuild
     ```

2. ビルド方法

   ```sh
   chmod +x compile.sh
   ./compile.sh
   ```

   または GPRbuild を使用:

   ```sh
   gprbuild -p -P anpr.gpr -XOS=UNIX
   ```

   または Make を使用:

   ```sh
   make
   ```

3. 実行方法

   ```sh
   bin/anpr
   ```

   **文字エンコーディングに関する注意:**
   Linux システムはデフォルトで UTF-8 をサポートしているため、日本語が正常に表示されます。問題がある場合は、ターミナルが UTF-8 エンコーディングに設定されているか確認してください。
