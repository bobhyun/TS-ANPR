[English](../../LICENSE.md) | [한국어](../ko-KR/LICENSE.md) | 日本語 | [Tiếng Việt](../vi-VN/LICENSE.md)

## ライセンス

#### 1. サンプルソースコード

提供されるサンプルソースコードは**MIT ライセンス**に従います。

```
The MIT License (MIT)
Copyright © 2022-2025 TS-Solution Corp.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to all conditions.

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

#### 2. TS-ANPR エンジン

TS-ANPR エンジンは**商用ライセンス**で提供されます。ライセンスの種類は以下の通りです。

| 区分                       | 機能<sup>(1)</sup>  |           多重認識数 | 性能<sup>(2)</sup> |           期間制限 |
| :------------------------- | :------------------ | -------------------: | -----------------: | -----------------: |
| `TS-ANPR 無料トライアル`   | `vmsdr`             |           最大 15 台 |   最大 16 CPU コア |              30 日 |
| `TS-ANPR Server`           | `vmsdr`             | 注文型<sup>(3)</sup> |           制限なし | ライセンスに準じる |
| `TS-ANPR Pro`              | `vmsdr`             |           最大 15 台 |   最大 16 CPU コア | ライセンスに準じる |
| `TS-ANPR Object Detection` | `msd`<sup>(4)</sup> |           最大 15 台 |    最大 8 CPU コア | ライセンスに準じる |
| `TS-ANPR Basic`            | `vdr`               |            最大 1 台 |    最大 8 CPU コア | ライセンスに準じる |

- <sup>(1)</sup> 機能区分
  - `v`: ナンバープレートの車両取り付けの有無を判断
  - `m`: 複数の車両のナンバープレートをすべて認識（多重認識）
  - `s`: 360° すべての角度の車両からナンバープレートを認識（サラウンド認識）
  - `d`: オブジェクト認識
  - `r`: 認識されたオブジェクト（車両）のナンバープレート認識
- <sup>(2)</sup> CPU コアの一部のみを使用する方式で、ナンバープレート認識性能を制限
- <sup>(3)</sup> 15 台から 500 台まで注文可能
- <sup>(4)</sup> オブジェクト認識機能のみで、車両ナンバープレート認識機能はなし
