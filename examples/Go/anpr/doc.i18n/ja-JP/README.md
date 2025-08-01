[English](../../) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# Go サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Go/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Go/anpr)

### 1. エンジンファイルのコピー

_**【参考】** この例では、他のサンプルとエンジンファイルを共有するために `examples/bin/` ディレクトリに展開しますが、実際の配布時には通常、アプリケーションの実行ファイルがあるディレクトリにエンジンファイルをコピーします。_

- Windows x86 64 ビット
  エンジンファイルを `examples/bin/windows-x86_64` ディレクトリに展開
  ```sh
  unzip tsanpr*-windows-x86_64.zip
  ```
- Windows x86 32 ビット
  エンジンファイルを `examples/bin/windows-x86` ディレクトリに展開
  ```sh
  unzip tsanpr*-windows-x86.zip
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
  └── Go
      └── anpr                   # source directory
         ├── anpr                # compiled executable for Windows
         ├── anpr.exe            # compiled executable for Linux
         ├── go.mod
         ├── main.go
         └── tsanpr
            ├── tsanpr_windows.go
            └── tsanpr_linux.go
  ```

### 2. ビルドと実行

#### 2.1 Windows (MinGW)

1. モジュールの初期化

   ```sh
   go mod tidy
   ```

2. 依存関係のインストール

   - GoCV 公式インストールガイドを参照してください。（https://gocv.io/getting-started/windows/）

3. ビルド方法

   ```sh
   go build -o anpr.exe main.go
   ```

4. 実行方法

   ```sh
   .\anpr.exe
   ```

#### 2.2. Linux

1. モジュールの初期化

   ```sh
   go mod tidy
   ```

2. 依存関係のインストール
   - Debian / Ubuntu Linux
     ```sh
     sudo apt install -y build-essential g++ libopencv-dev
     ```
   - Oracle Linux / RedHat (RHEL) / CentOS
     ```sh
     sudo dnf install -y epel-release gcc gcc-c++ opencv opencv-devel
     ```
3. ビルド方法

   ```sh
   go build -o anpr main.go
   ```

4. 実行方法

   ```sh
   ./anpr
   ```
