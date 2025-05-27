[English](../../LICENSE.md) | [日本語](../ja-JP/LICENSE.md) | [한국어](../ko-KR/LICENSE.md) | Tiếng Việt

## Giấy phép

#### 1. Mã nguồn ví dụ

Mã nguồn ví dụ được cung cấp tuân theo **Giấy phép MIT**.

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

#### 2. Động cơ TS-ANPR

Động cơ TS-ANPR được cung cấp theo **giấy phép thương mại**.
Các loại giấy phép như sau.

| Phân loại                       | Chức năng<sup>(1)</sup> |    Số lượng nhận diện đồng thời | Hiệu năng<sup>(2)</sup> | Giới hạn thời gian |
| :------------------------------ | :---------------------- | ------------------------------: | ----------------------: | -----------------: |
| `TS-ANPR Bản dùng thử miễn phí` | `vmsdr`                 |                    Tối đa 15 xe |       Tối đa 16 lõi CPU |            30 ngày |
| `TS-ANPR Server`                | `vmsdr`                 | Theo đơn đặt hàng<sup>(3)</sup> |          Không giới hạn |     Theo giấy phép |
| `TS-ANPR Pro`                   | `vmsdr`                 |                    Tối đa 15 xe |       Tối đa 16 lõi CPU |     Theo giấy phép |
| `TS-ANPR Object Detection`      | `msd`<sup>(4)</sup>     |                    Tối đa 15 xe |        Tối đa 8 lõi CPU |     Theo giấy phép |
| `TS-ANPR Basic`                 | `vdr`                   |                     Tối đa 1 xe |        Tối đa 8 lõi CPU |     Theo giấy phép |

- <sup>(1)</sup> Phân loại chức năng
  - `v`: Xác định biển số có gắn trên xe hay không
  - `m`: Nhận diện tất cả biển số xe trong ảnh (nhận diện đa phương tiện)
  - `s`: Nhận diện biển số xe ở mọi góc độ 360° (nhận diện toàn cảnh)
  - `d`: Nhận diện đối tượng
  - `r`: Nhận diện biển số xe của đối tượng (xe) đã nhận diện
- <sup>(2)</sup> Hiệu năng nhận diện biển số xe bị giới hạn bằng cách chỉ sử dụng một phần lõi CPU
- <sup>(3)</sup> Có thể đặt hàng từ 15 đến 500 xe
- <sup>(4)</sup> Chỉ có chức năng nhận diện đối tượng, không có chức năng nhận diện biển số xe
