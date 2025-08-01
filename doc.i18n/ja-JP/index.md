---
title: "ディープラーニングベース高精度ナンバープレート認識エンジン"
description: "ディープラーニングによる高精度ナンバープレート認識エンジン。日本、韓国、ベトナムのナンバープレート対応、魚眼レンズ、駐車場満空管理、リアルタイム検出、OCR、SDK、AIソリューション提供。"
keywords: "ナンバープレート認識, 車両ナンバー認識, ディープラーニング ナンバー認識, 車両番号OCR, AIナンバープレート, ナンバープレート検出, 自動認識エンジン, 駐車場ソリューション, 満空車管理, 魚眼レンズ, 車両管理, LPR, ALPR, ANPR, 車両入退場, 車両識別, 日本ナンバー, 韓国ナンバー, ベトナムナンバー, ナンバープレートSDK, リアルタイム認識, ナンバープレートAI, 駐車場管理, 番号認識, 車番認識, プレートリーダー"
author: "Bob Hyun <bobhyun@gmail.com>"
lang: "ja"
---

[English](/TS-ANPR/) | [한국어](/TS-ANPR/doc.i18n/ko-KR/) | 日本語 | [Tiếng Việt](/TS-ANPR/doc.i18n/vi-VN/)

# 😍TS-ANPR

**TS-ANPR**は、ディープラーニングベースの車両ナンバープレート認識エンジンで、韓国、日本、ベトナムのナンバープレート規格に対応しています。

##### ✨ アプリケーションの例 (TS-IVR)

<iframe width="720" height="405" src="https://www.youtube.com/embed/d7UU71PAx5Y" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

##### [😍 ライブデモ](http://tsnvr.ipdisk.co.kr/) <span style="font-size:.8em;font-weight:normal;color:grey">👈 ここで番号認識の性能を直接ご確認ください。</span>

##### [🚀 最新エンジンのダウンロード](https://github.com/bobhyun/TS-ANPR/releases/)

##### 🎨 人気のプログラミング言語によるコードサンプル

- [C](../../examples/C/anpr/)
- [C#](../../examples/C%23/anpr/)
- [C++](../../examples/C++/anpr/)
- [Clojure](../../examples/Clojure/anpr/)
- [Dart](../../examples/Dart/anpr/)
- [Delphi](../../examples/Delphi/anpr/)
- [F#](../../examples/F%23/anpr/)
- [Go](../../examples/Go/anpr/)
- [Haskell](../../examples/Haskell/anpr/)
- [Java](../../examples/Java/anpr/)
- [JavaScript](../../examples/JavaScript/anpr/)
- [Julia](../../examples/Julia/anpr/)
- [Kotlin](../../examples/Kotlin/anpr/)
- [Lua](../../examples/Lua/anpr/)
- [Perl](../../examples/Perl/anpr/)
- [Python](../../examples/Python/anpr/)
- [Ruby](../../examples/Ruby/anpr/)
- [Rust](../../examples/Rust/anpr/)
- [Scala](../../examples/Scala/anpr/)
- [Swift](../../examples/Swift/anpr/)
- [TypeScript](../../examples/TypeScript/anpr/)
- [VB.NET](../../examples/VB.NET/anpr/)

##### 📖 アプリ開発ガイド

- [TS-ANPR](DevGuide.md)
- [TS-CAM](https://github.com/bobhyun/TS-CAM/blob/main/DevGuide.md)

##### [🎁 インストール手順](Usage.md)

##### [⚖️ ライセンス](license_page.md)

_ご質問やご要望がございましたら、[Issues](https://github.com/bobhyun/TS-ANPR/issues) にお気軽にご投稿ください。
皆さまからのフィードバックやご連絡をお待ちしております！_

- お問い合わせ: 📧 skju3922@naver.com

---

## 目次

- [最新バージョン情報](#最新バージョン情報)
- [ディープラーニングモデルの種類とその用途](#ディープラーニングモデルの種類とその用途)
- [ディープラーニングモデル別認識速度比較表](#ディープラーニングモデル別認識速度比較表)
- [特長点](#特長点)
- [多様な認識オプション](#多様な認識オプション)

<br/>

---

## 最新バージョン情報

#### v3.1.0 リリース (2025.8.1)🎉

1. 認識率の向上
   - 日本および韓国のナンバープレート認識率が向上しました。

## ディープラーニングモデルの種類とその用途

ライセンスはすべてのディープラーニングモデルに適用されるため、用途に最も適したモデルを選択するだけで構いません。

| モデル |      認識速度       | 適用例                                                                                                                                                                                                                                                  |
| :----: | :-----------------: | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| **S**  | 速い<br/>(近距離用) | 駐車場の入出管理<br/><img src="../../img/models/small1.png" />                                                                                                                                                                                          |
| **M**  | 普通<br/>(中距離用) | 駐車スペース満空管理／駐車位置検索<br/><img src="../../img/models/medium1.png" /><br/>魚眼レンズカメラ（360° 全方位認識）<br/><img src="../../img/models/medium2.png" /><br/>転覆車両（360° 全方位認識）<br/><img src="../../img/models/medium3.png" /> |
| **L**  | 遅い<br/>(長距離用) | 大規模屋外駐車場／車両台数カウント<br/><img src="../../img/models/large1.png" /><br/>多車線車両ナンバープレート認識<br/><img src="../../img/models/large2.png" /><br/>通行量集計<br/><img src="../../img/models/large3.png" />                          |

## ディープラーニングモデル別認識速度比較表

| CPU                               | コア数 | スレッド数 | クロック<sup>(1)</sup> | OS                                    | S<sup>(2)</sup> | M<sup>(2)</sup> | L<sup>(2)</sup> |
| --------------------------------- | -----: | ---------: | ---------------------: | :------------------------------------ | --------------: | --------------: | --------------: |
| Intel i7-12700                    |     12 |         20 |                    2.1 | 64 ビット Windows<br/>64 ビット Linux |           0.021 |           0.036 |           0.054 |
| Intel i5-6500                     |      4 |          4 |                    3.2 | 64 ビット Windows<br/>64 ビット Linux |           0.031 |           0.078 |           0.140 |
| (同上)                            |        |            |                        | 32 ビット Windows                     |           0.078 |           0.172 |           0.296 |
| Intel i3-8100                     |      4 |          4 |                    3.6 | 64 ビット Windows<br/>64 ビット Linux |           0.042 |           0.087 |           0.156 |
| (同上)                            |        |            |                        | 32 ビット Windows                     |           0.089 |           0.204 |           0.656 |
| Intel Celeron J4005               |      2 |          2 |                    2.0 | 64 ビット Windows<br/>64 ビット Linux |           0.396 |           0.886 |           1.563 |
| (同上)                            |        |            |                        | 32 ビット Windows                     |           0.629 |           1.355 |           2.368 |
| Intel Celeron 1037U<sup>(3)</sup> |      2 |          2 |                    1.8 | 32 ビット Windows                     |           0.484 |           1.061 |           1.856 |
| Rockchip RK3588S<sup>(4)</sup>    |      8 |          8 |                    1.5 | 64 ビット Linux                       |           0.227 |           0.462 |           0.842 |
| Broadcom BCM2711<sup>(5)</sup>    |      4 |          4 |                    1.8 | 64 ビット Linux                       |           0.465 |           1.024 |           1.817 |

- 車両が一台だけ写っている画像で測定
- <sup>(1)</sup> 単位: GHz
- <sup>(2)</sup> 単位: 秒
- <sup>(3)</sup> 32 ビット専用 CPU [(メーカー仕様を見る)](https://www.intel.co.kr/content/www/kr/ko/products/sku/71995/intel-celeron-processor-1037u-2m-cache-1-80-ghz/specifications.html)
- <sup>(4)</sup> NanoPi R6S [(メーカー仕様を見る)](https://www.friendlyelec.com/index.php?route=product/product&product_id=289)
- <sup>(5)</sup> Raspberry Pi4 [(メーカー仕様を見る)](https://www.raspberrypi.com/products/raspberry-pi-4-model-b/)

## 特長点

#### 1. 車両番号認識性能

下記のようなさまざまな環境要因への適応力に優れています。

- 反射フィルム（韓国ナンバープレート）
  <div>
    <img style="margin-right:-5px" width="120" src="../../img/ex/film1.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film2.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film3.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film4.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film5.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film6.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film7.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film8.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film9.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film10.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film11.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film12.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film13.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/film14.jpg" />
  <div>
- 夜間ノイズ
  <div>
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise1.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise2.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise3.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise4.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise5.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise6.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise7.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise8.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/noise9.jpg" />
  <div>
- 撮影角度
  <div>
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle1.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle3.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle2.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle5.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle4.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle6.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle7.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle9.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle8.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle10.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle11.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/angle12.jpg" />
  </div>
- 天候／照明
  <div>
    <img style="margin-right:-5px" width="120" src="../../img/ex/light1.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light6.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light3.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light4.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light5.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light2.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light8.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light7.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light9.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/light10.jpg" />
  </div>
- 汚れ／損傷
  <div>    
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty1.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty2.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty3.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty4.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty5.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty6.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty7.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty10.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty9.jpg" />
    <img style="margin-right:-5px" width="120" src="../../img/ex/dirty8.jpg" />
  </div>
- 360 度魚眼カメラ画像。
  - _画像を展開（デウォーピング）せず、元の画像から複数の車両ナンバーを認識します。_
  <div>
    <img style="margin-right:-5px" src="../../img/ex/fisheye1.jpg" />
  </div>

#### 2. 各種ナンバープレート規格に対応

下記のようなさまざまなナンバープレート規格に対応しています。

- 韓国ナンバープレート

  - ダンプトラック・建設機械ナンバープレート
    <div>    
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq1.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq2.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq3.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq4.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq5.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq6.jpg" />
    </div>
  - 特殊ナンバープレート（仮、外交、軍用）
    <div>
      <img style="margin-right:-5px" width="120" src="../../img/ex/temp1.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/temp2.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/temp3.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/temp4.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/dep1.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/dep2.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/dep3.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/dep4.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/dep5.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/mil1.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/mil2.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/mil3.jpg" />
    </div>
  - 環境に優しい電気自動車ナンバープレート
    - ナンバープレート認識結果で環境に優しい電気自動車かどうかを判別します。
    - ただし、営業用車両ナンバープレートのように、ナンバープレート規格上、内燃機関車両と区別できない場合は判別できません。
    <div>
      <img style="margin-right:-5px" width="120" src="../../img/ex/ev2.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/ev1.jpg" />
    </div>
  - 1980 年代・1990 年代の旧型ナンバープレート
    - 1996 年のナンバープレート規格改定以前に使用されていた「처」「퍼」「차」「파」「추」～「후」、「그」～「흐」の文字に対応しています。
    - 旧型の在韓米軍ナンバープレート形式に対応しています。
    <div>    
      <img style="margin-right:-5px" width="120" src="../../img/ex/801.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/802.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/803.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/804.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/805.jpg" />
    </div>

- 日本のナンバープレート
  - 特殊ナンバープレート（外交用、自衛隊用）
    <div>    
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-mil1.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-mil2.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-mil3.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-mil4.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-mil5.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-dep1.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-dep2.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-dep3.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-dep4.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-dep5.jpg" />
    </div>
  - 1960 年代の旧型ナンバープレート
    - 地域名を一文字（例：東、京、名など）で表記していた旧型ナンバープレート形式に対応しています。
    <div>    
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-601.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-602.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-603.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-604.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-605.jpg" />
    </div>

#### 3. 主なオペレーティングシステム／CPU アーキテクチャ対応

- Windows
  - Intel 系 64 ビット（windows-x86_64）、32 ビット（windows-x86）対応
  - Windows 7 以降に対応
- Linux
  - Intel 系 64 ビット（linux-x86_64）対応
  - ARM 系 64 ビット（linux-aarch64）対応
  - ディストリビューションを問わず、glibc 2.27 以降に対応

#### 4. 多様な開発環境に対応

- 特定のプログラミング言語に依存しない汎用ライブラリインターフェース
  - [プログラミング言語ごとのサンプルを提供](../../examples/)
- [入力画像ファイル形式](DevGuide.md#12-anpr_read_file)
  - `bmp`, `jpg`, `png`, `pnm`, `pbm`, `pgm`, `ppm`, `jfif`, `webp`
- [入力画像メモリバッファのピクセル形式](DevGuide.md#13-anpr_read_pixels)
  - `GRAY`, `BGRA`, `RGBA`, `RGB`, `BGR`, `BGR555`, `BGR565`, `HSV`, `YCrCb`, `I420`, `YV12`, `IYUV`, `NV12`, `NV21`
- [認識結果の出力形式](DevGuide.md#2-output-format)
  - `text`, `csv`, `json`, `yaml`, `xml`

#### 5. 多様なライセンスを提供

- 無料評価版ライセンス
  - 開発およびデモ用に、システムごとにインストール後 30 日間の無料使用期間を提供
- 商用ライセンス
  - メディア別：USB ドングルまたはソフトウェアライセンスから選択可能
  - 機能および性能別：アプリケーションソフトウェアの要件に応じて、`Basic`、`Object Detection`、`Pro`、`Server` から選択可能（参考：[TS-ANPR エンジン](LICENSE.md#2-ts-anpr-エンジン)）

## 多様な認識オプション

#### 1. 車両に取り付けられたナンバープレートの検査

車体が写っている画像で、ナンバープレートが車両に取り付けられているかどうかを判別します。
**車両取り付け(v)** オプションを使用すると、車両に取り付けられたナンバープレートのみを認識します。<br/>
<img width="500" src="../../img/mounted1.jpg" />

下の画像のように車両が写っていないナンバープレートやバイクのナンバープレートなどは無視されます。<br/>

<img width="500" src="../../img/mounted2.jpg">
<div style="font-size:0.8em">[画像出典: 연합뉴스]</div>
</img>

<br/>

<img width="500" src="../../img/mounted2-1.jpg">
<div style="font-size:0.8em">[画像出典: 바이커즈랩]</div>
</img>

<br/>

ナンバープレートだけが近接撮影されている場合、車両認識ができないことがありますが、そのような場合は **車両取り付け(v)** オプションを無効に設定すると車両の番号の認識が出来ます。<br/>
<img width="500" src="../../img/mounted3.jpg" />

#### 2. 複数認識

**複数認識(m)** オプションを使用すると、画像内に複数の車両がある場合、すべて認識されます。<br/>
<img width="800" src="../../img/multiple1.jpg" />

**複数認識(m)** オプションを使用しない場合、複数の車両の中で最もナンバープレートの信頼度が高い（よく見える）ものだけが認識されます。<br/>
<img width="800" src="../../img/multiple2.jpg" />

#### 3. 360° サラウンド認識

**360° サラウンド認識(s)** オプションを使用すると、画像内の車両が転倒していたり、魚眼レンズカメラで撮影された車両など、あらゆる方向に傾いている場合でもナンバープレートを認識できます。<br/>

<img width="800" src="../../img/surround1.jpg">
<div style="font-size:0.8em">[画像出典: KBS]</div>
</img>

<br/>

<img width="800" src="../../img/surround2.jpg" />

#### 4. オブジェクト検出

**オブジェクト検出(d)** オプションを使用すると、画像内のオブジェクトを検出します。
検出された車両領域とアプリケーションで設定した駐車スペース領域を比較することで、満車か空車かを判断できます。<br/>

<img width="800" src="../../img/options/dms.png" />

#### 5. オブジェクト（車両）のナンバープレート認識

**オブジェクト検出(d)** と **ナンバープレート認識(r)** オプションを併用すると、検出された車両のナンバープレートも認識します。<br/>

<img width="800" src="../../img/options/dmsr.png" />

#### 5. 関心領域および最小ナンバープレートサイズの設定

**関心領域(i)**、**非関心領域(x)**、**最小ナンバープレートサイズ(a)** を組み合わせて設定することで、関心領域外の車両のナンバープレート認識を防ぐことができます。<br/>

<img width="800" src="../../img/options/roi.png" />

---

- アプリケーション開発前の基本的な性能テストには、[ライブデモ](http://tsnvr.ipdisk.co.kr/)をご利用いただけます。
- アプリケーション開発段階では、[アプリケーション開発ガイド](DevGuide.md) と一緒に同梱されてるプログラミング言語別サンプルをご参照ください。
