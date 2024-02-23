TS-ANPR
===

TS-ANPR은 딥러닝 기반의 대한민국 차량 번호 인식 엔진입니다.

*아래 **차번 인식 데모** 사이트에서 가지고 계신 차량 이미지로 직접 테스트해 보세요.*
#### [🚘 차번 인식 데모](http://tsnvr.ipdisk.co.kr/)

#### [⚡️ 최신 엔진 다운로드](https://github.com/bobhyun/TS-ANPR/releases/)

#### [💻 응용 프로그램 개발 가이드](DevGuide.md) 

- [Entry points](DevGuide.md#1-entry-points)
- [pdf 문서 다운로드](https://github.com/bobhyun/TS-ANPR/blob/main/doc/TS-ANPR-Manual.pdf)

#### [🎁 설치 방법](Usage.md)


## 최신 버전 정보
#### v1.7.2 출시🎉 (2024-02-23)
1. 인식률 향상
2. 구형 주한미군 번호판 지원<br/>
![](https://i.namu.wiki/i/wwI1jJQAIZlh_gSD3Vt-2rmuIzYkQ4BNTNTLWv6GU9RMTL01ujgvhxYpFKR0ckzqa-q6_O4L4v0V8AUliVczf7INwNgsbw3DBnDZlkk8aRzGVqkLovKDVfdxkhNYEZqpn4Z90-AeizRDVzFNriHWSQ.webp)
<br/>`[이미지 출처: 나무위키]`


#### v1.7.1 출시🎉 (2024-02-07)
1. 저사양 CPU용 `lite` 버전 출시
    - 인식 속도는 일반 버전에 비해 약 2배 빠름
    - 인식률은 일반 버전에 비해 근거리 촬영 이미지는 차이가 거의 없고 원거리 촬영이나 다중 인식시는 소폭 떨어짐
      - 주차장 입출구 용으로 사용 권장
    - 엔진 파일만 교체하고 라이선스 변경없이 사용 가능
2. 인식률 향상
    - 30도 이상 기울어진 번호판 이미지에서 인식률 향상

## 인식 속도 비교표
|CPU                             | 코어| 쓰레드|    클럭|    운영체제     |실행환경 | 일반 버전 | lite 버전|
|--------------------------------|----:|-----:|-------:|----------------|-------:|---------:|----------:|
|인텔 i7-12700                   |  12 |   20 | 2.1GHz | 윈도우즈, 리눅스 | 64비트 |  0.040초 |    0.021초|
|인텔 i5-6500                    |   4 |    4 | 3.2GHz | 윈도우즈, 리눅스 | 64비트 |  0.094초 |    0.031초|
| *(상동)*                       |     |      |        | 윈도우즈        | 32비트 |  0.203초 |    0.094초|
|인텔 i3-8100                    |   4 |    4 | 3.6GHz | 윈도우즈, 리눅스 | 64비트 |  0.099초 |    0.042초|
| *(상동)*                       |     |      |        | 윈도우즈        | 32비트 |  0.204초 |    0.096초|
|인텔 셀러론 J4005                |   2 |    2 | 2.0GHz | 윈도우즈, 리눅스 | 64비트 |  1.017초 |    0.398초|
| *(상동)*                       |     |      |        | 윈도우즈         | 32비트 |  1.571초 |    0.649초|
|인텔 셀러론 1037U *(32비트 전용)*|   2 |    2 | 1.8GHz | 윈도우즈         | 32비트 |  1.186초 |    0.484초|
|Rockchip RK3588S                |   8 |    8 | 1.5GHz | 리눅스 (ARM)    | 64비트 |  0.534초 |    0.247초|
|BCM2711 *(라즈베리 파이4)*       |   4 |    4 | 1.8GHz | 리눅스 (ARM)    | 64비트 |  1.213초 |    0.528초|

## 특장점
#### 1. 차번 인식 능력
아래와 같은 다양한 환경 요인에 대해 뛰어난 적응력을 보입니다.
- 반사 필름
  <div>
    <img style="margin-right:-5px" width="120" src="img/ex/film1.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/film2.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/film3.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/film4.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/film5.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/film6.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/film7.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/film8.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/film9.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/film10.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/film11.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/film12.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/film13.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/film14.jpg" />
  <div>
- 야간 노이즈
  <div>
    <img style="margin-right:-5px" width="120" src="img/ex/noise1.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/noise2.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/noise3.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/noise4.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/noise5.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/noise6.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/noise7.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/noise8.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/noise9.jpg" />
  <div>
- 촬영 각도
  <div>
    <img style="margin-right:-5px" width="120" src="img/ex/angle1.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/angle3.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/angle2.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/angle5.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/angle4.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/angle6.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/angle7.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/angle9.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/angle8.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/angle10.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/angle11.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/angle12.jpg" />
  </div>
- 날씨 / 조명
  <div>
    <img style="margin-right:-5px" width="120" src="img/ex/light1.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/light6.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/light3.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/light4.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/light5.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/light2.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/light8.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/light7.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/light9.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/light10.jpg" />
  </div>
- 오염 / 훼손
  <div>    
    <img style="margin-right:-5px" width="120" src="img/ex/dirty1.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/dirty2.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/dirty3.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/dirty4.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/dirty5.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/dirty6.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/dirty7.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/dirty10.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/dirty9.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/dirty8.jpg" />
  </div>
- 360도 어안 카메라 이미지
  - *이미지를 펼치지 않고 원본 이미지에서 여러 대의 차량 번호를 인식합니다.*
  <div>
    <img style="margin-right:-5px" src="img/ex/fisheye1.jpg" />
  </div>

#### 2. 각종 번호판 지원
아래와 같은 다양한 번호판 규격을 지원합니다.
- 덤프트럭, 중장비 번호판
  <div>    
    <img style="margin-right:-5px" width="120" src="img/ex/eq1.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/eq2.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/eq3.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/eq4.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/eq5.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/eq6.jpg" />
  </div>
- 특수 번호판 (임시, 외교, 군용)
  <div>
    <img style="margin-right:-5px" width="120" src="img/ex/temp1.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/temp2.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/temp3.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/temp4.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/dep1.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/dep2.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/dep3.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/dep4.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/dep5.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/mil1.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/mil2.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/mil3.jpg" />
  </div>
- 친환경 전기차 번호판
  - *차번 인식 결과 데이터의 `ev`항목에 `true` 또는 `false`로 구분합니다.*
  - *단, 영업용 차량 번호판처럼 번호판 규격상 내연기관 차량과 구분되지 않는 경우는 판단이 불가능합니다.*
  <div>
    <img style="margin-right:-5px" width="120" src="img/ex/ev2.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/ev1.jpg" />
  </div>
- ’80, ’90년대 구형 번호판
  - *1996년도 번호판 규격 개정 이전에 사용되던 `처`, `퍼`, `차`, `파`, `추` ~ `후`, `그` ~ `흐` 문자를 지원합니다.*
  <div>    
    <img style="margin-right:-5px" width="120" src="img/ex/801.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/802.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/803.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/804.jpg" />
  </div>
#### 3. 주요 운영체제 / CPU 아키텍처 지원
- 윈도우즈
  - 인텔 계열 64비트(`windows-x86_64`), 32비트(`windows-x86`)
  - 윈도우즈 7 이상 호환
- 리눅스
  - 인텔 계열 64비트(`linux-x86_64`), 
  - ARM 계열 64비트(`linux-aarch64`)
  - 배포판에 관계없이 `glibc 2.27` 이상 호환

#### 4. 다양한 개발 환경 지원
- 특정 프로그래밍 언어에 종속되지 않는 범용 라이브러리 인터페이스
- [프로그래밍 언어별 예제 제공](DevGuide.md#4-%EC%98%88%EC%A0%9C) (C, C++, C#, Visual Basic, Python, JavaScript/Node.js, Go, Pascal/Delphi, Perl, Ruby)
- [입력 이미지 파일 형식](DevGuide.md#12-anpr_read_file) (bmp, jpg, png, pnm, pbm, pgm, ppm, jfif, webp)
- [입력 이미지 메모리 버퍼 픽셀 형식](DevGuide.md#13-anpr_read_pixels) (GRAY, BGRA, RGBA, RGB, BGR, BGR555, BGR565, HSV, YCrCb, I420, YV12, IYUV, NV12, NV21)
- [인식 결과 출력 형식](DevGuide.md#2-output-format) (text, json, yaml, xml, csv)

#### 5. 다양한 라이선스 제공
- 무료 평가판 라이선스
  - 개발 및 데모용으로 시스템당 설치 이후 30일간 무료 사용 기간 제공
- 상용 라이선스
  - 매체별: USB 동글, 또는 소프트웨어 라이선스 중 선택
  - 기능 및 성능별: `IoT`, `Basic`, `Pro`, `Server` 중 응용 소프트웨어 요구사항에 따라 선택 가능 ([자세히](LICENSE.md#2-ts-anpr-%EC%97%94%EC%A7%84-%EB%B0%94%EC%9D%B4%EB%84%88%EB%A6%AC))

## 인식 옵션
#### 1. 차량 장착 검사 (Vehicle Mounted)
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

#### 2. 다중 인식 (Multiple Recognition)
**다중 인식(m)** 옵션을 사용하면 이미지에 차량이 여러 대 있으면 모두 인식합니다.
<br/>![](img/multiple1.jpg)

**다중 인식(m)** 옵션을 사용하지 않으면 여러 대 차량 중 가장 번호판 신뢰도가 높은(잘 보이는) 것 하나만 인식합니다.
<br/>![](img/multiple2.jpg)


#### 3. 서라운드 인식 (Surround Recognition)
**서라운드 인식(s)** 옵션을 사용하면 전복된 차량 또는 어안 렌즈 카메라로 촬영한 차량 등 이미지 내의 차량이 사방으로 기울어져 있거나 넘어져 있는 경우도 차량 번호를 인식할 수 있습니다.
<br/>![](img/surround1.jpg)
<br/>`[이미지 출처: KBS]`
<br/>![](img/surround2.jpg)



- 응용 프로그램 개발 전 단계의 기본적인 성능 테스트는 [온라인 데모 사이트 http://tsnvr.ipdisk.co.kr/](http://tsnvr.ipdisk.co.kr/) 를 이용하실 수 있습니다.
- 응용 프로그램 개발 단계에서는 [응용 프로그램 개발 가이드](DevGuide.md) 와 포함된 프로그래밍 언어별 예제들을 참고하시기 바랍니다.
- 개발 관련 질문이나 요청 사항들은 [Issues](https://github.com/bobhyun/TS-ANPR/issues)에 등록해 주시면 적극적으로 지원하겠습니다.


<br/>

- 개발 문의: bobhyun@gmail.com
- 구매 문의: skju3922@naver.com 
- 📞 전화: <a href="tel:02-6084-3920">02-6084-3920</a>
  
<br/>

