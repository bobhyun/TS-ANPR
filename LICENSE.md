## 라이선스

#### 1. 예제 소스 코드
제공되는 예제 소스 코드는 **MIT 라이선스**를 따릅니다.

```
The MIT License (MIT)
Copyright © 2022 TS-Solution Corp.

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


#### 2. TS-ANPR 엔진 바이너리
TS-ANPR 엔진 바이너리는 **상용 라이선스**로 제공됩니다.
라이선스 종류는 아래와 같습니다.

|         구분         |  기능<sup>(1)</sup>  | 다중 인식 수   | 성능<sup>(2)</sup>| 기간 제한     |
|:---------------------|:-------------------|--------------:|-----------------:|--------------:|
| `TS-ANPR 무료 평가판` | `vmsdr`            | 최대 15대    | 최대 16 CPU 코어  | 30일          |
| `TS-ANPR 서버`       | `vmsdr`            | 주문형<sup>(3)</sup> | 제한없음    | 라이선스에 준함 |
| `TS-ANPR 프로`       | `vmsdr`            | 최대 15대     | 최대 16 CPU 코어 | 라이선스에 준함 |
| `TS-ANPR 객체인식`   | `msd`<sup>(4)</sup> | 최대 15대     | 최대 8 CPU 코어  | 라이선스에 준함 |
| `TS-ANPR 기본`       | `vdr`              | 최대 1대      | 최대 8 CPU 코어  | 라이선스에 준함 |
| `TS-ANPR IoT`        | `vdr`              | 최대 1대      | 최대 4 CPU 코어  | 라이선스에 준함 |

- <sup>(1)</sup> 기능 구분
  - `v`: 번호판 차량 부착 여부 판단
  - `m`: 여러 대의 차량 번호판을 모두 인식 (다중 인식)
  - `s`: 360° 모든 각도의 차량에서 번호판 인식 (서라운드 인식)
  - `d`: 객체 인식
  - `r`: 인식된 객체(차량)의 차량 번호 인식
- <sup>(2)</sup> CPU 코어 일부분만 사용하는 방식으로 차량 번호 인식 성능을 제한
- <sup>(3)</sup> 15대부터 500대까지 구매 가능
- <sup>(4)</sup> 객체 인식 기능만 있고 차량 번호 인식 기능은 없음

