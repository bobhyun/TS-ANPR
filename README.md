TS-ANPR
===

TS-ANPR은 딥러닝 기반의 대한민국 차량 번호 인식 엔진입니다.
#### 차번 인식 데모: [http://tsnvr.ipdisk.co.kr/](http://tsnvr.ipdisk.co.kr/) 
<img style="margin-left:20px" src="img/demo.png" width="180" />

#### 최신 엔진 다운로드: [https://github.com/bobhyun/TS-ANPR/releases/](https://github.com/bobhyun/TS-ANPR/releases/)
<img style="margin-left:20px" src="img/releases.png" width="180" />

  - `[2022-12-19]` v1.0.6 출시
    - 1996년 개정 후 폐지된 구형 번호판 (녹색 바탕 명조체 한글) 인식률 개선
     
     ![구형 번호판](https://file.namu.moe/file/8bc9e381797334eb33da66e3ba501be1ad5c3c044c21c39367dcb1e762010283710d7dbcc810d0b56495c3b5e55133837c61b6089d641e0c33286ddc98261995)
     `[이미지 출처: 나무위키]`
  - `[2022-10-28]` v1.0.5 출시
    - 응용에서 멀티스레드로 병렬 호출시 메모리 사용량 및 스레드 스위칭 오버헤드 개선 
    - 영업용 번호판 한글 문자 (`바`, `사`, `아`, `자`) 오인식 개선
#### 응용 프로그램 개발 가이드: [https://github.com/bobhyun/TS-ANPR/blob/main/DevGuide.md](https://github.com/bobhyun/TS-ANPR/blob/main/DevGuide.md) 
<img style="margin-left:20px" src="img/dev-guide.png" width="180" />

- [DLL entry points](https://github.com/bobhyun/TS-ANPR/blob/main/DevGuide.md#1-dll-entry-points)
- [입력 이미지 파일 형식 (`bmp`, `jpg`, `png`, `pnm`, `pbm`, `pgm`, `ppm`, `jfif`, `webp`)](https://github.com/bobhyun/TS-ANPR/blob/main/DevGuide.md#12-anpr_read_file)
- [입력 이미지 픽셀 형식 (`GRAY`, `BGRA`, `RGBA`, `RGB`, `BGR`, `BGR555`, `BGR565`, `HSV`, `YCrCb`, `I420`, `YV12`, `IYUV`, `NV12`, `NV21`)](https://github.com/bobhyun/TS-ANPR/blob/main/DevGuide.md#13-anpr_read_pixels)
- [결과 출력 형식 (`text`, `json`, `yaml`, `xml`)](https://github.com/bobhyun/TS-ANPR/blob/main/DevGuide.md#2-output-format)
- [프로그래밍 언어별 예제 소스 코드 (`C/C++`, `C#`, `Visual Basic`, `Python`, `JavaScript/Node.js`, `Go`, `Pascal/Delphi`, `Perl`, `Ruby`)](https://github.com/bobhyun/TS-ANPR/blob/main/DevGuide.md#4-%EC%98%88%EC%A0%9C)
- [pdf 문서 다운로드](https://github.com/bobhyun/TS-ANPR/raw/main/TS-ANPR-Manual.pdf)


<br/>

- 개발 문의: bobhyun@gmail.com
- 구매 문의: skju3922@naver.com 
- 📞 전화: <a href="tel:02-6084-3922">02-6084-3922</a>
  
<br/>


## 특징

### 1. 차량 장착 검사 (Vehicle Mounted)
차체가 보이는 이미지에서 차량에 장착된 번호판인지 구분합니다.
**차량 장착(v)** 옵션을 사용하면 차량에 장착된 번호판만 인식합니다.
<br/>![](img/mounted1.jpg)

아래 이미지처럼 차량없이 번호판만 있거나 바이크 번호판 등은 무시합니다.
<br/>![](img/mounted2.jpg)
<br/>`[이미지 출처: 연합뉴스]`
<br/>![](img/mounted2-1.jpg)
<br/>`[이미지 출처: 바이커즈랩]`

번호판만 근접 촬영된 경우는 차량 인식이 안되는 경우가 있는데, 이런 경우 **차량 장착(v)** 옵션을 사용하지 않으면 차량 번호를 인식할 수 있습니다.
<br/>![](img/mounted3.jpg)


### 2. 다중 인식 (Multiple Recognition)
**다중 인식(m)** 옵션을 사용하면 이미지에 차량이 여러 대 있으면 모두 인식합니다.
<br/>![](img/multiple1.jpg)

**다중 인식(m)** 옵션을 사용하지 않으면 여러 대 차량 중 가장 번호판 신뢰도가 높은(잘 보이는) 것 하나만 인식합니다.
<br/>![](img/multiple2.jpg)


### 3. 서라운드 인식 (Surround Recognition)
**서라운드 인식(s)** 옵션을 사용하면 전복된 차량 또는 어안 렌즈 카메라로 촬영한 차량 등 이미지 내의 차량이 사방으로 기울어져 있거나 넘어져 있는 경우도 차량 번호를 인식할 수 있습니다.
<br/>![](img/surround1.jpg)
<br/>`[이미지 출처: KBS]`
<br/>![](img/surround2.jpg)

## 라이선스

### 1. 예제 소스 코드
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

### 2. TS-ANPR 엔진 바이너리
TS-ANPR 엔진 바이너리는 **상용 라이선스**로 제공됩니다.
아래 세 종류의 라이선스를 사용할 수 있습니다.

|         구분         |  기능                                       | 기간 제한  |
|:---------------------|:-------------------------------------------|:----------:|
| `TS-ANPR 무료 평가판` | 차량 장착(v), 다중 인식(m), 서라운드 인식(s)  | 30일      |
| `TS-ANPR 기본`       | 차량 장착(v), 단일 인식                      | 라이선스에 준함     |
| `TS-ANPR 프로`       | 차량 장착(v), 다중 인식(m), 서라운드 인식(s)  | 라이선스에 준함     |

