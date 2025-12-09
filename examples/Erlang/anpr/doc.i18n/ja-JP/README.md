[English](../../README.md) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# Erlang サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Erlang/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Erlang/anpr)

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
  └── Erlang
      └── anpr                   # プロジェクトディレクトリ
         ├── c_src               # C NIF ソースファイル
         │   └── tsanpr_nif.c
         ├── src                 # Erlang ソースファイル
         │   ├── anpr_app.erl    # アプリケーション動作
         │   ├── anpr_sup.erl    # スーパーバイザー動作
         │   ├── anpr.erl        # メインモジュール
         │   ├── tsanpr.erl      # NIFラッパーモジュール
         │   └── anpr.app.src    # アプリケーションリソースファイル
         ├── priv                # コンパイルされたNIFライブラリ
         │   └── tsanpr_nif.dll/.so
         ├── rebar.config        # rebar3設定
         └── _build              # rebar3ビルド出力（生成）
            └── default
               └── lib
                  └── anpr
                     ├── ebin    # コンパイルされた.beamファイル
                     └── priv    # NIFライブラリ
  ```

### 2. 前提条件

1. Erlang/OTP のインストール（バージョン 24 以降推奨）

   **Windows:**

   ```sh
   # https://www.erlang.org/downloads からダウンロード
   # または Chocolatey を使用
   choco install erlang
   ```

   **Linux:**

   ```sh
   # Ubuntu/Debian
   sudo apt-get update
   sudo apt-get install -y erlang

   # Oracle Linux / RHEL / CentOS (8/9+)
   sudo dnf install -y erlang || sudo yum install -y erlang
   # ヒント: パッケージが見つからない場合は EPEL を有効化、または Erlang Solutions リポジトリを利用：
   # https://www.erlang.org/downloads

   # または kerl を使用（推奨）
   curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
   chmod a+x kerl
   ./kerl build 24.3 24.3
   ./kerl install 24.3 ~/erlang/24.3
   # 現在のシェルで有効化
   . ~/erlang/24.3/activate
   # 無効化するには: deactivate
   ```

2. rebar3のインストール

   **Windows:**

   GitHubから最新のrebar3 escriptをダウンロード：
   ```powershell
   # 最新のrebar3をダウンロード (Erlang/OTP 28+互換)
   Invoke-WebRequest -Uri "https://github.com/erlang/rebar3/releases/latest/download/rebar3" -OutFile "rebar3"
   
   # rebar3をプロジェクトディレクトリに配置するかPATH内のディレクトリに移動
   ```

   **注意:** Windowsではrebar3は`escript rebar3 <コマンド>`の形式で実行する必要があります（`rebar3 <コマンド>`ではありません）。

   **Linux:**

   ```sh
   # Ubuntu/Debian
   sudo apt-get install rebar3
   
   # またはGitHubから最新版をダウンロード
   wget https://github.com/erlang/rebar3/releases/latest/download/rebar3
   chmod +x rebar3
   sudo mv rebar3 /usr/local/bin/
   ```

3. Cコンパイラのインストール（NIFビルドに必要）

   **Windows:**
   - C++開発ツールを含むVisual Studioをインストール、または
   - MinGW-w64をインストール：`choco install mingw`

   **Linux:**
   ```sh
   # Ubuntu/Debian
   sudo apt-get install build-essential

   # Fedora/CentOS
   sudo yum groupinstall "Development Tools"
   ```

4. インストールの確認

   ```sh
   erl -version
   rebar3 version
   gcc --version  # WindowsでVisual Studioを使用する場合はcl.exe
   ```

### 3. ビルドと実行

1. Erlangサンプルディレクトリに移動

   ```sh
   cd examples/Erlang/anpr
   ```

2. NIF (Native Implemented Function) ライブラリのビルド

   **Windows:**

   「x64 Native Tools Command Prompt for VS」を開くか、vcvars64.batを実行後：
   ```cmd
   build_nif.bat
   ```

   以下を実行します：
   - Erlangインストールを自動検出
   - MSVCを使用して`c_src/tsanpr_nif.c`をコンパイル
   - `priv/tsanpr_nif.dll`を生成

   **Linux:**

   ```sh
   make priv/tsanpr_nif.so
   ```

   以下を実行します：
   - gccを使用して`c_src/tsanpr_nif.c`をコンパイル
   - libdlを適切にリンクして`priv/tsanpr_nif.so`を生成

3. Erlangアプリケーションのビルド

   **Windows (escriptコマンドを使用):**
   ```cmd
   escript rebar3 compile
   ```

   **Linux:**
   ```sh
   rebar3 compile
   ```

   以下を実行します：
   - Erlangモジュール（`anpr.erl`、`tsanpr.erl`）を`_build/default/lib/anpr/ebin/`にコンパイル
   - `priv/`のNIFライブラリを`_build/default/lib/anpr/priv/`にコピー

4. アプリケーションの実行

   ```sh
   # 非対話モード
   erl -pa _build/default/lib/anpr/ebin -noshell -eval "anpr:main()" -s init stop

   # 対話モード
   erl -pa _build/default/lib/anpr/ebin
   1> anpr:main().
   ```

### 4. 注意事項

- この Erlang 実装は他の言語サンプルと同じ機能を提供します
- Erlang の NIF（Native Implemented Functions）を使用してネイティブ TSANPR ライブラリとインターフェースします
- Erlang の耐障害性と並行性モデルにより高可用性システムに理想的です
- Windows と Linux のクロスプラットフォームサポート
- 注意：これはデモンストレーション目的の簡略化された NIF 実装です

### 5. 機能

- **ファイルベース認識**: 画像ファイルを直接処理
- **エンコード画像処理**: エンコードされた画像データの処理（JPEG、PNG など）
- **ピクセルバッファ処理**: 生ピクセルデータの処理（簡略化された実装）
- **複数出力形式**: テキスト、JSON、YAML、XML、CSV 出力をサポート
- **複数認識モード**: 単一プレート、複数プレート、車両検出など
- **関心領域（RoI）**: 画像内の特定領域を処理
- **多国対応**: 異なるナンバープレート形式をサポート（KR、JP、VN など）

### 6. API リファレンス

#### tsanpr モジュール

`tsanpr`モジュールは以下の関数を提供します：

**初期化:**

- `tsanpr:new(LibraryPath)`: ネイティブライブラリパスで初期化

**コア関数:**

- `tsanpr:anpr_initialize(Tsanpr, Mode)`: ANPR エンジンを初期化
- `tsanpr:anpr_read_file(Tsanpr, ImgFileName, OutputFormat, Options)`: 画像ファイルを処理
- `tsanpr:anpr_read_pixels(Tsanpr, Pixels, Width, Height, Stride, PixelFormat, OutputFormat, Options)`: ピクセルデータを処理

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

### 7. トラブルシューティング

**ライブラリ読み込みの問題:**

- TSANPR ライブラリパスが正しいことを確認
- すべてのシステム依存関係がインストールされていることを確認
- ライブラリの権限が正しいことを確認

**NIF の問題:**

- このサンプルはデモンストレーション用の簡略化された NIF アプローチを使用
- 本番使用には適切な C NIF の実装が必要
- Erlang 開発ヘッダーがインストールされていることを確認

**プラットフォーム固有の問題:**

- **Windows**: Visual C++ 再頒布可能パッケージがインストールされていることを確認
- **Linux**: 必要なシステムライブラリをインストールし、ライブラリの権限が正しいことを確認
