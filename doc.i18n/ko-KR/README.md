[English](../../README.md) | 한국어 | [日本語](../ja-JP/README.md) | [Tiếng Việt](../vi-VN/README.md)

# 😍TS-ANPR

**TS-ANPR**은 딥러닝 기반의 차량 번호 인식 엔진으로 대한민국, 일본, 베트남 번호판 규격을 지원합니다.

##### [😍 라이브 데모](http://tsnvr.ipdisk.co.kr/) <span style="font-size:.7em;font-weight:normal;color:grey">👈 여기서 번호 인식 성능을 직접 확인해 보세요.</span>

##### [🚀 최신 엔진 다운로드](https://github.com/bobhyun/TS-ANPR/releases/)

##### 🎨 프로그래밍 언어별 예제 코드

| # | 언어 | 인기도 | # | 언어 | 인기도 |
|--:|:---------|:-----------|--:|:---------|:-----------|
| 1 | [Python](../../examples/Python/anpr/) | ⭐⭐⭐⭐⭐ | 2 | [JavaScript](../../examples/JavaScript/anpr/) | ⭐⭐⭐⭐⭐ |
| 3 | [TypeScript](../../examples/TypeScript/anpr/) | ⭐⭐⭐⭐⭐ | 4 | [Java](../../examples/Java/anpr/) | ⭐⭐⭐⭐⭐ |
| 5 | [C#](../../examples/C%23/anpr/) | ⭐⭐⭐⭐ | 6 | [C++](../../examples/C++/anpr/) | ⭐⭐⭐⭐ |
| 7 | [Go](../../examples/Go/anpr/) | ⭐⭐⭐⭐ | 8 | [Rust](../../examples/Rust/anpr/) | ⭐⭐⭐⭐ |
| 9 | [C](../../examples/C/anpr/) | ⭐⭐⭐⭐ | 10 | [Swift](../../examples/Swift/anpr/) | ⭐⭐⭐ |
| 11 | [Kotlin](../../examples/Kotlin/anpr/) | ⭐⭐⭐ | 12 | [Ruby](../../examples/Ruby/anpr/) | ⭐⭐⭐ |
| 13 | [Dart](../../examples/Dart/anpr/) | ⭐⭐⭐ | 14 | [Scala](../../examples/Scala/anpr/) | ⭐⭐⭐ |
| 15 | [Perl](../../examples/Perl/anpr/) | ⭐⭐ | 16 | [Lua](../../examples/Lua/anpr/) | ⭐⭐ |
| 17 | [Haskell](../../examples/Haskell/anpr/) | ⭐⭐ | 18 | [Elixir](../../examples/Elixir/anpr/) | ⭐⭐ |
| 19 | [Erlang](../../examples/Erlang/anpr/) | ⭐⭐ | 20 | [F#](../../examples/F%23/anpr/) | ⭐⭐ |
| 21 | [Julia](../../examples/Julia/anpr/) | ⭐⭐ | 22 | [Zig](../../examples/Zig/anpr/) | ⭐⭐ |
| 23 | [Clojure](../../examples/Clojure/anpr/) | ⭐⭐ | 24 | [VB.NET](../../examples/VB.NET/anpr/) | ⭐⭐ |
| 25 | [Crystal](../../examples/Crystal/anpr/) | ⭐ | 26 | [Delphi](../../examples/Delphi/anpr/) | ⭐ |
| 27 | [D](../../examples/D/anpr/) | ⭐ | 28 | [Fortran](../../examples/Fortran/anpr/) | ⭐ |
| 29 | [Ada](../../examples/Ada/anpr/) | ⭐ | 30 | [COBOL](../../examples/COBOL/anpr/) | ⭐ |

##### 📖 응용 프로그램 개발 가이드

- [TS-ANPR](DevGuide.md)
- [TS-CAM](https://github.com/bobhyun/TS-CAM/blob/main/DevGuide.md)

##### [🎁 설치 방법](Usage.md)

##### [⚖️ 라이선스](LICENSE.md)

##### ✨ 응용 프로그램 예 (TS-IVR) <span style="font-size:.7em;font-weight:normal;color:grey"> [😎 영상 전체 보기](https://www.youtube.com/watch?v=d7UU71PAx5Y)</span>

https://github.com/user-attachments/assets/71a2977a-4d1f-479b-a909-21c03fd9f013

_질문이나 요청이 있으시면 언제든지 [Issues](https://github.com/bobhyun/TS-ANPR/issues) 남겨주세요.
여러분의 소중한 의견과 피드백을 항상 환영합니다!_

- 문의: 📧 skju3922@naver.com ☎️ [02-6084-3920](tel:02-6084-3920)

---

## 목차

- [최신 버전 정보](#최신-버전-정보)
- [딥러닝 모델별 용도](#딥러닝-모델별-용도)
- [딥러닝 모델별 인식 속도 비교표](#딥러닝-모델별-인식-속도-비교표)
- [특장점](#특장점)
- [다양한 인식 옵션](#다양한-인식-옵션)

<br/>

---

## 최신 버전 정보

#### v3.1.5 출시 (2025.12.9)🎉
1. 한국 번호판 인식률 향상

#### v3.1.4 출시 (2025.10.17)🎉

1. `anpr_read_pixels` 함수에서 `stride` 값이 음수인 경우 오인식 문제 수정
2. `r` 옵션없이 `d` 옵션을 사용하여 객체인식하는 경우 오토바이 객체가 결과에서 생략되는 문제 수정

#### v3.1.3 출시 (2025.9.29)🎉

1. 한국 번호판 인식률 향상 (특히 `S` 모델이 많이 향상됨)
2. 매번 새로운 스레드에서 번호인식 호출되는 경우 발생하는 `103: Too many workers` 오류 방지
   - 종료된 스레드에 대한 자원을 자동 정리하도록 개선하여 동시에 256개 이상의 스레드를 모두 사용하는 경우가 아닌 이상 발생하지 않도록 함

#### v3.1.2 출시 (2025.8.29)🎉

1. `sync` 파라메터로 엔진을 초기화한 다음, 다중 스레드에서 경쟁적으로 차번인식을 호출하는 경우 크래쉬 발생 가능성이 있어 수정함

#### v3.1.1 출시 (2025.8.18)🎉

1. `dr` 옵션을 사용하여 차량과 차량 번호를 동시에 인식할 때, 번호판이 보이지 않는 차량이 결과에서 제외되는 문제가 있어 수정함.

#### v3.1.0 출시 (2025.8.1)🎉

1. 인식률 향상
   - 일본 및 한국 번호판 인식률이 향상되었습니다.

## 딥러닝 모델별 용도

라이선스는 모든 딥러닝 모델에 공통으로 적용되며 용도에 적합한 모델을 선택하면 됩니다.

| 모델  |      처리 속도      | 적용 예                                                                                                                                                                                                                                                        |
| :---: | :-----------------: | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **S** | 빠름<br/>(근거리용) | 주차장 입출관리<br/><img src="../../img/models/small1.png" />                                                                                                                                                                                                  |
| **M** | 보통<br/>(중거리용) | 주차면 만.공차 관리 / 주차 위치 찾기<br/><img src="../../img/models/medium1.png" /><br/>어안 렌즈 카메라 (360° 서라운드 인식)<br/><img src="../../img/models/medium2.png" /><br/>전복 차량 (360° 서라운드 인식)<br/><img src="../../img/models/medium3.png" /> |
| **L** | 느림<br/>(원거리용) | 대규모 야외 주차장 / 차량 대수 카운트<br/><img src="../../img/models/large1.png" /><br/>다차로 차량 번호 인식<br/><img src="../../img/models/large2.png" /><br/>통행량 집계<br/><img src="../../img/models/large3.png" />                                      |

## 딥러닝 모델별 인식 속도 비교표

| CPU                             | 코어 | 쓰레드 | 클럭<sup>(1)</sup> | 운영체제                          | S<sup>(2)</sup> | M<sup>(2)</sup> | L<sup>(2)</sup> |
| ------------------------------- | ---: | -----: | -----------------: | :-------------------------------- | --------------: | --------------: | --------------: |
| 인텔 i7-12700                   |   12 |     20 |                2.1 | 64비트 윈도우즈<br/>64비트 리눅스 |           0.021 |           0.036 |           0.054 |
| 인텔 i5-6500                    |    4 |      4 |                3.2 | 64비트 윈도우즈<br/>64비트 리눅스 |           0.031 |           0.078 |           0.140 |
| (상동)                          |      |        |                    | 32비트 윈도우즈                   |           0.078 |           0.172 |           0.296 |
| 인텔 i3-8100                    |    4 |      4 |                3.6 | 64비트 윈도우즈<br/>64비트 리눅스 |           0.042 |           0.087 |           0.156 |
| (상동)                          |      |        |                    | 32비트 윈도우즈                   |           0.089 |           0.204 |           0.656 |
| 인텔 셀러론 J4005               |    2 |      2 |                2.0 | 64비트 윈도우즈<br/>64비트 리눅스 |           0.396 |           0.886 |           1.563 |
| (상동)                          |      |        |                    | 32비트 윈도우즈                   |           0.629 |           1.355 |           2.368 |
| 인텔 셀러론 1037U<sup>(3)</sup> |    2 |      2 |                1.8 | 32비트 윈도우즈                   |           0.484 |           1.061 |           1.856 |
| Rockchip RK3588S<sup>(4)</sup>  |    8 |      8 |                1.5 | 64비트 리눅스                     |           0.227 |           0.462 |           0.842 |
| Broadcom BCM2711<sup>(5)</sup>  |    4 |      4 |                1.8 | 64비트 리눅스                     |           0.465 |           1.024 |           1.817 |

- 차량 한대만 있는 이미지로 측정함
- <sup>(1)</sup> 단위: GHz
- <sup>(2)</sup> 단위: 초
- <sup>(3)</sup> 32비트 전용 CPU [(제조사 사양 보기)](https://www.intel.co.kr/content/www/kr/ko/products/sku/71995/intel-celeron-processor-1037u-2m-cache-1-80-ghz/specifications.html)
- <sup>(4)</sup> NanoPi R6S [(제조사 사양 보기)](https://www.friendlyelec.com/index.php?route=product/product&product_id=289)
- <sup>(5)</sup> 라즈베리 파이4 [(제조사 사양 보기)](https://www.raspberrypi.com/products/raspberry-pi-4-model-b/)

## 특장점

#### 1. 차번 인식 성능

아래와 같은 다양한 환경 요인에 대해 뛰어난 적응력을 보입니다.

- 반사 필름 (한국 번호판)
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
- 야간 노이즈
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
- 촬영 각도
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
- 날씨 / 조명
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
- 오염 / 훼손
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
- 360도 어안 카메라 이미지
  - _이미지를 펼치지 않고 원본 이미지에서 여러 대의 차량 번호를 인식합니다._
  <div>
    <img style="margin-right:-5px" src="../../img/ex/fisheye1.jpg" />
  </div>

#### 2. 각종 번호판 규격 지원

아래와 같은 다양한 번호판 규격을 지원합니다.

- 한국 번호판

  - 덤프트럭, 중장비 번호판
    <div>    
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq1.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq2.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq3.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq4.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq5.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/eq6.jpg" />
    </div>
  - 특수 번호판 (임시, 외교, 군용)
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
  - 친환경 전기차 번호판
    - 차번 인식 결과에 친환경 전기차 여부를 구분합니다.
    - 단, 영업용 차량 번호판처럼 번호판 규격상 내연기관 차량과 구분되지 않는 경우는 판단이 불가능합니다.
    <div>
      <img style="margin-right:-5px" width="120" src="../../img/ex/ev2.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/ev1.jpg" />
    </div>
  - ’80, ’90년대 구형 번호판

    - 1996년도 번호판 규격 개정 이전에 사용되던 ‘처’, ‘퍼’, ‘차’, ‘파’, ‘추’ ~ ‘후’, ‘그’ ~ ‘흐’ 문자를 지원합니다.
    - 구형 주한미군 번호판 형식을 지원합니다.
    <div>    
      <img style="margin-right:-5px" width="120" src="../../img/ex/801.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/802.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/803.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/804.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/805.jpg" />
    </div>

- 일본 번호판
  - 특수 번호판 (외교, 자위대)
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
  - ’60년대 구형 번호판
    - 지역명 한 글자(예: 東, 京, 名 등)만 표기하던 구형 번호판 형식을 지원합니다.
    <div>    
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-601.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-602.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-603.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-604.jpg" />
      <img style="margin-right:-5px" width="120" src="../../img/ex/jp-605.jpg" />
    </div>

#### 3. 주요 운영체제 / CPU 아키텍처 지원

- 윈도우즈
  - 인텔 계열 64비트(windows-x86_64), 32비트(windows-x86)
  - 윈도우즈 7 이상 호환
- 리눅스
  - 인텔 계열 64비트(linux-x86_64),
  - ARM 계열 64비트(linux-aarch64)
  - 배포판에 관계없이 glibc 2.27 이상 호환

#### 4. 다양한 개발 환경 지원

- 특정 프로그래밍 언어에 종속되지 않는 범용 라이브러리 인터페이스
  - [프로그래밍 언어별 예제 제공](../../examples/)
- [입력 이미지 파일 형식](DevGuide.md#12-anpr_read_file)
  - `bmp`, `jpg`, `png`, `pnm`, `pbm`, `pgm`, `ppm`, `jfif`, `webp`
- [입력 이미지 메모리 버퍼 픽셀 형식](DevGuide.md#13-anpr_read_pixels)
  - `GRAY`, `BGRA`, `RGBA`, `RGB`, `BGR`, `BGR555`, `BGR565`, `HSV`, `YCrCb`, `I420`, `YV12`, `IYUV`, `NV12`, `NV21`
- [인식 결과 출력 형식](DevGuide.md#3-output-formats)
  - `text`, `csv`, `json`, `yaml`, `xml`

#### 5. 다양한 라이선스 제공

- 무료 평가판 라이선스
  - 개발 및 데모용으로 시스템당 설치 이후 30일간 무료 사용 기간 제공
- 상용 라이선스
  - 매체별: USB 동글, 또는 소프트웨어 라이선스 중 선택
  - 기능 및 성능별: `Basic`, `객체인식`, `Pro`, `Server` 중 응용 소프트웨어 요구사항에 따라 선택 가능 ([참고: TS-ANPR 엔진](LICENSE.md#2-ts-anpr-엔진))

## 다양한 인식 옵션

#### 1. 차량에 장착된 번호판 검사

차체가 보이는 이미지에서 차량에 부착된 번호판인지 구분합니다.
**차량 부착(v)** 옵션을 사용하면 차량에 부착된 번호판만 인식합니다.<br/>
<img width="500" src="../../img/mounted1.jpg" />

아래 이미지처럼 차량없이 번호판만 있거나 바이크 번호판 등은 무시합니다.<br/>

<img width="500" src="../../img/mounted2.jpg">
<div style="font-size:0.8em">[이미지 출처: 연합뉴스]</div>
</img>

<br/>

<img width="500" src="../../img/mounted2-1.jpg">
<div style="font-size:0.8em">[이미지 출처: 바이커즈랩]</div>
</img>

<br/>

번호판만 근접 촬영된 경우는 차량 인식이 안되는 경우가 있는데, 이런 경우 **차량 부착(v)** 옵션을 사용하지 않으면 차량 번호를 인식할 수 있습니다.<br/>
<img width="500" src="../../img/mounted3.jpg" />

#### 2. 다중 인식 (Multiple Recognition)

**다중 인식(m)** 옵션을 사용하면 이미지에 차량이 여러 대 있으면 모두 인식합니다.<br/>
<img width="800" src="../../img/multiple1.jpg" />

**다중 인식(m)** 옵션을 사용하지 않으면 여러 대 차량 중 가장 번호판 신뢰도가 높은(잘 보이는) 것 하나만 인식합니다.<br/>
<img width="800" src="../../img/multiple2.jpg" />

#### 3. 360° 서라운드 인식 (Surround Recognition)

**360° 서라운드 인식(s)** 옵션을 사용하면 전복된 차량 또는 어안 렌즈 카메라로 촬영한 차량 등 이미지 내의 차량이 사방으로 기울어져 있거나 넘어져 있는 경우도 차량 번호를 인식할 수 있습니다.<br/>

<img width="800" src="../../img/surround1.jpg">
<div style="font-size:0.8em">[이미지 출처: KBS]</div>
</img>

<br/>

<img width="800" src="../../img/surround2.jpg" />

#### 4. 객체 인식 (Object Detection)

**객체 인식(d)** 옵션을 사용하면 이미지 내의 객체를 인식합니다.
출력된 차량 영역과 응용 프로그램에서 설정한 주차면 영역을 비교하면 만.공차 여부를 판단할 수 있습니다.<br/>

<img width="800" src="../../img/options/dms.png" />

#### 5. 객체(차량)의 차량 번호 인식 (Read License Plate)

**객체 인식(d)** 과 **차량 번호 인식(r)** 옵션을 함께 사용하면 객체 인식된 차량의 번호까지 인식합니다.<br/>

<img width="800" src="../../img/options/dmsr.png" />

#### 5. 관심 영역 및 최소 번호판 크기 설정

**관심 영역(i)**, **비관심 영역(x)**, **최소 번호판 크기(a)**를 조합해서 설정하여 관심 영역 밖의 차량이 번호 인식되는 경우를 방지할 수 있습니다.<br/>

<img width="800" src="../../img/options/roi.png" />

---

- 응용 프로그램 개발 전 단계의 기본적인 성능 테스트는 [라이브 데모](http://tsnvr.ipdisk.co.kr/)를 이용하실 수 있습니다.
- 응용 프로그램 개발 단계에서는 [응용 프로그램 개발 가이드](DevGuide.md) 와 포함된 프로그래밍 언어별 예제들을 참고하시기 바랍니다.
