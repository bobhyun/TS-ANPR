[English](../../) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# Ruby サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Ruby/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Ruby/anpr)

### 1. エンジンファイルのコピー

_**【参考】** この例では、他のサンプルとエンジンファイルを共有するために `examples/bin/` ディレクトリに展開しますが、実際の配布時には通常、アプリケーションの実行ファイルがあるディレクトリにエンジンファイルをコピーします。_

- Windows x86 64 ビット
  エンジンファイルを `examples/bin/windows-x86_64` ディレクトリに展開
  ```sh
  unzip tsanpr*-windows-x86_64.zip
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
  │   ├─── windows-x86_64        # engine directory for windows (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   ├── linux-x86_64           # engine directory for linux (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # engine directory for linux (arm64)
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # image directory
  └── Ruby
      └── anpr                   # source directory
         ├── anpr.rb
         └── lib
            └── tsanpr.rb
  ```

### 2. 依存関係のインストールと実行

1. 依存関係のインストール

   1. Windows

      ```sh
      # Chocolatey の使用を推奨します。
      choco install vips
      gem install ffi ruby-vips
      ```

   2. Linux

      ```sh
      # Debian / Ubuntu Linux
      sudo apt-get install -y libvips

      # Oracle Linux / RedHat (RHEL) / CentOS
      sudo yum install vips

      sudo gem install ffi ruby-vips
      ```

2. 実行方法

   ```sh
   ruby anpr.rb
   ```
