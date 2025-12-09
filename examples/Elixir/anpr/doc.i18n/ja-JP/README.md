[English](../../README.md) | [한국어](../ko-KR/) | 日本語 | [Tiếng Việt](../vi-VN/)

# Elixir サンプル

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Elixir/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Elixir/anpr)

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
  └── Elixir
      └── anpr                   # プロジェクトディレクトリ
         ├── c_src               # C NIF ソース
         │   └── tsanpr_nif.c
         ├── lib                 # Elixir ソース
         │   ├── anpr.ex         # メインモジュール
         │   └── tsanpr.ex       # NIF ラッパーモジュール
         ├── priv                # NIF ライブラリ（ビルド成果物）
         │   └── tsanpr_nif.dll/.so
         ├── mix.exs             # Mix プロジェクト設定
         ├── Makefile            # ビルド設定（Linux）
         ├── Makefile.win        # ビルド設定（Windows, nmake）
         ├── build_nif.bat       # ビルドスクリプト（Windows）
         └── _build              # Mix ビルド出力（生成物）
  ```

### 2. 前提条件

1. Elixir のインストール（バージョン 1.12 以降推奨）

   **Windows:**

   ```sh
   # Chocolatey を使用
   choco install elixir

   # または https://elixir-lang.org/install.html#windows からダウンロード
   ```

   **Linux:**

   ```sh
   # Ubuntu/Debian
   sudo apt-get install elixir

   # または asdf を使用（推奨）
   asdf plugin add elixir
   asdf install elixir latest
   ```

2. インストールの確認

   ```sh
   elixir --version
   ```

### 3. ビルドと実行

1. Elixir サンプルディレクトリに移動

   ```sh
   cd Elixir/anpr
   ```

2. NIF のビルド（ネイティブライブラリ）

   **Windows:**

   「x64 Native Tools Command Prompt for VS 2022」を開き、`cl`/`link` を PATH に用意してから:

   ```cmd
   build_nif.bat
   ```

   これにより以下を行います:
   - Erlang インストールの自動検出
   - MSVC による `c_src/tsanpr_nif.c` のコンパイル
   - `priv/tsanpr_nif.dll` の作成

   備考: `mix compile` が Windows で `elixir_make` 経由で `nmake` を起動する場合、`Makefile.win` が実行され、内部で `build_nif.bat` を呼び出して Windows のクォート問題を回避します。

   **Linux:**

   ```sh
   make priv/tsanpr_nif.so
   ```

   - gcc による `c_src/tsanpr_nif.c` のコンパイル
   - `priv/tsanpr_nif.so` の作成

3. 依存関係のインストールと Elixir アプリのコンパイル

   ```sh
   mix deps.get
   mix compile
   ```

4. 実行

   ```sh
   # Mix を利用
   mix run -e "ANPR.main()"

   # あるいは iex（対話型）
   iex -S mix
   iex> ANPR.main()
   ```

### 4. 注意事項

- この Elixir 実装は他の言語サンプルと同じ機能を提供します
- Elixir の NIF（Native Implemented Functions）を使用してネイティブ TSANPR ライブラリとインターフェースします
- Elixir の耐障害性設計とアクターモデルにより分散システムに優れています
- Windows と Linux のクロスプラットフォームサポート
- 注意：これはデモンストレーション目的の簡略化された NIF 実装です

追加の実装メモ:
- Windows では `ANPR.get_engine_file_name/0` が 64bit BEAM（`:erlang.system_info(:wordsize) == 8`）の場合に `examples/bin/windows-x86_64/tsanpr.dll` を選択し、それ以外は `windows-x86` を選択します。
- NIF モジュールは `c_src/tsanpr_nif.c` で `Elixir.TSANPR` として登録され、`lib/tsanpr.ex` ではベース名（`tsanpr_nif`）でロードするため、プラットフォーム拡張子が自動選択されます。

### 5. 機能

- **ファイルベース認識**: 画像ファイルを直接処理
- **エンコード画像処理**: エンコードされた画像データの処理（JPEG、PNG など）
- **ピクセルバッファ処理**: 生ピクセルデータの処理（簡略化された実装）
- **複数出力形式**: テキスト、JSON、YAML、XML、CSV 出力をサポート
- **複数認識モード**: 単一プレート、複数プレート、車両検出など
- **関心領域（RoI）**: 画像内の特定領域を処理
- **多国対応**: 異なるナンバープレート形式をサポート（KR、JP、VN など）

### 6. API リファレンス

#### TSANPR モジュール

`TSANPR`モジュールは以下の関数を提供します：

**初期化:**

- `TSANPR.new(library_path)`: ネイティブライブラリパスで初期化

**コア関数:**

- `TSANPR.anpr_initialize(tsanpr, mode)`: ANPR エンジンを初期化
- `TSANPR.anpr_read_file(tsanpr, img_file_name, output_format, options)`: 画像ファイルを処理
- `TSANPR.anpr_read_pixels(tsanpr, pixels, width, height, stride, pixel_format, output_format, options)`: ピクセルデータを処理

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
