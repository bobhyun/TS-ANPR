English | [한국어](doc.i18n/ko-KR/LICENSE.md) | [日本語](doc.i18n/ja-JP/LICENSE.md) | [Tiếng Việt](doc.i18n/vi-VN/LICENSE.md)

## License

#### 1. Example Source Code

The provided example source code is licensed under the **MIT License**.

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

#### 2. TS-ANPR Engine

The TS-ANPR engine is provided under a **commercial license**. The types of licenses are as follows:

| Types                      | Features<sup>(1)</sup> | Multi-Recognition Count | Performance<sup>(2)</sup> |     Duration Limit |
| :------------------------- | :--------------------- | ----------------------: | ------------------------: | -----------------: |
| `TS-ANPR Free Trial`       | `vmsdr`                |       Up to 15 vehicles |        Up to 16 CPU cores |            30 days |
| `TS-ANPR Server`           | `vmsdr`                |    Custom<sup>(3)</sup> |                 Unlimited | Subject to license |
| `TS-ANPR Pro`              | `vmsdr`                |       Up to 15 vehicles |        Up to 16 CPU cores | Subject to license |
| `TS-ANPR Object Detection` | `msd`<sup>(4)</sup>    |       Up to 15 vehicles |         Up to 8 CPU cores | Subject to license |
| `TS-ANPR Basic`            | `vdr`                  |         Up to 1 vehicle |         Up to 8 CPU cores | Subject to license |

- <sup>(1)</sup> Features
  - `v`: Determines whether a vehicle has a license plate
  - `m`: Recognizes all license plates of multiple vehicles (multi-recognition)
  - `s`: Recognizes license plates from vehicles at all angles (surround recognition)
  - `d`: Object detection
  - `r`: Recognizes the vehicle number of detected objects (vehicles)
- <sup>(2)</sup> Limits vehicle number recognition performance by using only a portion of CPU cores
- <sup>(3)</sup> Orders can be placed for quantities ranging from 15 to 500 units.
- <sup>(4)</sup> Only has object detection functionality and does not include vehicle number recognition functionality.
