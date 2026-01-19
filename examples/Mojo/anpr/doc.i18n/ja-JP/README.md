[English](../../README.md) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# Mojo サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Mojo/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Mojo/anpr)

### 1. エンジンファイルのコピー

_**[注意]** Mojoは現在Linuxのみをサポートしています。このサンプルでは、他のサンプルと共有するためにエンジンファイルを examples/bin/ディレクトリに展開します。ただし、実際のデプロイメントでは、通常、アプリケーションの実行ファイルが配置されているディレクトリにエンジンファイルをコピーします。_

- Linux x86 64 ビット用
  エンジンファイルを`examples/bin/linux-x86_64`ディレクトリに展開
  ```sh
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- Linux arm 64 ビット用
  エンジンファイルを`examples/bin/linux-aarch64`ディレクトリに展開
  ```sh
  tar xvf tsanpr*-linux-aarch64.tar.xz
  ```
- ディレクトリ構造
  ```sh
  examples
  ├── bin
  │   ├── linux-x86_64           # Linux (x86_64)用エンジンディレクトリ
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # Linux (arm64)用エンジンディレクトリ
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # 画像ディレクトリ
  └── Mojo
      └── anpr                   # プロジェクトディレクトリ
          ├── pixi.toml
          └── src                # ソースディレクトリ
              ├── anpr.mojo
              ├── webcam.mojo
              └── tsanpr.mojo
  ```

### 2. 実行方法

1. Mojo SDK のインストール

   - pixi（パッケージマネージャー）のインストール
     ```sh
     curl -fsSL https://pixi.sh/install.sh | sh
     source ~/.bashrc  # または ~/.zshrc
     ```

   - pixi を使用して Mojo をインストール（pixi.toml に設定済み）
     ```sh
     cd examples/Mojo/anpr
     pixi install
     ```

   詳細は https://docs.modular.com/mojo/manual/install/ を参照してください。

2. `anpr`の実行

   ```sh
   pixi run anpr
   ```

3. `webcam`の実行

   ```sh
   pixi run webcam
   ```
