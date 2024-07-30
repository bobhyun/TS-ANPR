TS-ANPR
===

TS-ANPR은 딥러닝 기반의 대한민국 차량 번호 인식 엔진입니다.

---

#### [😍 차번 인식 라이브 데모](http://tsnvr.ipdisk.co.kr/) <span style="font-size:.7em;font-weight:normal;color:grey">👈 여기서 번호 인식 성능을 테스트해 보세요.</span>

#### [🚀 최신 엔진 다운로드](https://github.com/bobhyun/TS-ANPR/releases/)

#### [💻 응용 프로그램 개발 가이드](DevGuide.md) 

#### [🎁 설치 방법](Usage.md)

#### [⚖️ 라이선스](LICENSE.md)
---

## 목차
  - [최신 버전 정보](#최신-버전-정보)
  - [딥러닝 모델별 용도](#딥러닝-모델별-용도)
  - [딥러닝 모델별 인식 속도 비교](#딥러닝-모델별-인식-속도-비교)
  - [특장점](#특장점)
  - [다양한 인식 옵션](#다양한-인식-옵션)

<br/>

*관련 질문이나 요청 사항은 [Issues](https://github.com/bobhyun/TS-ANPR/issues)에 등록해 주시면 적극 지원하겠습니다.*


- 개발 문의: bobhyun@gmail.com
- 구매 문의: skju3922@naver.com 
- 📞 전화: <a href="tel:02-6084-3920">02-6084-3920</a>
  
---

## 최신 버전 정보
#### v2.3.0 출시 (2024.7.30)🎉 
1. 인코딩된 이미지 버퍼 입력 지원
  `jpg`, `png` 등 인코딩된 이미지 버퍼를 입력으로 차번인식하는 방식을 지원합니다.
  *(네트워크로 전송받은 이미지를 파일에 저장하거나 디코딩하지 않고 즉시 번호인식하는 경우 유용합니다.)*
```python
  # 파이썬 예제
  result = anpr_read_pixels(
    buffer,       # 이미지 시작 주소
    length,       # 이미지 바이트 수
    0,            # 사용안함
    0,            # 사용안함
    'jpg',        # 압축 이미지 형식
    'json',       # 번호인식 결과 출력 형식 
    'vms'         # 번호인식 옵션
  )
```
2. 인식률 향상 

#### v2.1.3 출시 (2024.6.18)🎉 
1. 인식 성능 향상
    - 자동차 후면 근접 촬영 이미지에서 미인식 현상 개선
    - 덤프트럭 번호판 인식률 향상
2. 오류 수정
    - 차량 영역이 겹쳐진 경우 동일 차량 번호 중복 출력 문제 수정

#### v2.1.0 출시 (2024.4.29)🎉 
1. 객체 인식 기능 추가
    - 360° 어안렌즈 카메라로 촬영한 외곡된 형태의 원본 이미지에서 객체 및 차량 번호 인식
    - 차량 영역을 인식하여 주차면 만.공차 판별용으로 활용
    - 다중 인식 기능으로 이미지 내의 차량 대수 카운트용으로 활용
    - 인식 가능한 객체 종류: 차, 오토바이
2. 라이선스 모델 확장
    - 기존 번호 인식 라이선스에 객체 인식 기능 포함
    - 라이선스별 최대 인식 객체 수 제한
    - 주차면 만.공차 판별 전용 `TS-ANPR 객체인식` 출시 (번호 인식 기능은 없고 객체 인식 기능만 있음)
      ([참고: TS-ANPR 엔진 바이너리](LICENSE.md#2-ts-anpr-엔진-바이너리))
3. 딥러닝 모델 파일 구성 변경
    - 기존 두 개의 `*.eon` 파일을 하나로 합치고 용도별로 아래와 같이 구분함
      | 모델  | 파일명                 | 비고
      |:-----:|-----------------------|--------
      | **S** | `tsanpr-KR-*S.eon`   | 기존 `lite` 모델 명칭 변경
      | **M** | `tsanpr-KR-*M.eon`   | 기존 일반 모델 명칭 변경
      | **L** | `tsanpr-KR-*L.eon`   | 신규 출시
4. 인식률 향상
5. `M` 모델 인식 속도 향상 (약 15% 단축)
6. 인식 결과 데이터에 포함된 숫자를 소숫점 이하 네 자리 까지만 표기
7. 친환경 전기차 구분용 `ev` 값을 `attrs`로 옮김 ([참고: 2.1. 차량 번호 인식 결과](DevGuide.md#213-json))


## 딥러닝 모델별 용도
라이선스는 모든 딥러닝 모델에 공통으로 적용되며 용도에 적합한 모델을 선택하면 됩니다.

| 모델  | 성능   | 용도 예시
|:-----:|:-----:|:---------------------------------------------
| **S** | 속도 빠름<br/>(근거리용) |주차장 입출관리<br/><img src="img/models/small1.png" />
| **M** | 속도 보통<br/>(중거리용) |주차면 만.공차 관리 / 주차 위치 찾기<br/><img src="img/models/medium1.png" /><br/>어안 렌즈 카메라 (360° 서라운드 인식)<br/><img src="img/models/medium2.png" /><br/>전복 차량 (360° 서라운드 인식)<br/><img src="img/models/medium3.png" />
| **L** | 속도 느림<br/>(원거리용) |대규모 야외 주차장 / 차량 대수 카운트<br/><img src="img/models/large1.png" /><br/>다차로 차량 번호 인식<br/><img src="img/models/large2.png" /><br/>통행량 집계<br/><img src="img/models/large3.png" />


## 딥러닝 모델별 인식 속도 비교
|CPU                 | 코어| 쓰레드| 클럭<sup>(1)</sup>| 운영체제 | S<sup>(2)</sup>| M<sup>(2)</sup>| L<sup>(2)</sup>|
|--------------------|----:|-----:|-------:|:--------|------:|------:|------:|
|인텔 i7-12700        |  12 |   20 | 2.1| 64비트 윈도우즈<br/>64비트 리눅스| 0.021| 0.036| 0.054
|인텔 i5-6500        |   4 |    4 | 3.2| 64비트 윈도우즈<br/>64비트 리눅스 | 0.031| 0.078| 0.140
| (상동)             |     |      |    | 32비트 윈도우즈        | 0.078| 0.172| 0.296
|인텔 i3-8100        |   4 |    4 | 3.6| 64비트 윈도우즈<br/>64비트 리눅스 | 0.042| 0.087| 0.156
| (상동)             |     |      |    | 32비트 윈도우즈        | 0.089| 0.204| 0.656
|인텔 셀러론 J4005   |   2 |    2 | 2.0| 64비트 윈도우즈<br/>64비트 리눅스 | 0.396| 0.886| 1.563
| (상동)             |     |      |    | 32비트 윈도우즈         | 0.629| 1.355| 2.368
|인텔 셀러론 1037U<sup>(3)</sup>|   2 |    2 | 1.8| 32비트 윈도우즈         | 0.484| 1.061| 1.856
|RK3588S<sup>(4)</sup>|   8 |    8 | 1.5| 64비트 리눅스    | 0.227| 0.462| 0.842
|BCM2711<sup>(5)</sup>|   4 |    4 | 1.8| 64비트 리눅스    | 0.465| 1.024| 1.817
- 차량 한대만 있는 이미지로 측정함
- <sup>(1)</sup> 단위: GHz
- <sup>(2)</sup> 단위: 초
- <sup>(3)</sup> 32비트 전용 CPU [(제조사 사양 보기)](https://www.intel.co.kr/content/www/kr/ko/products/sku/71995/intel-celeron-processor-1037u-2m-cache-1-80-ghz/specifications.html)
- <sup>(4)</sup> NanoPi R6S [(제조사 사양 보기)](https://www.friendlyelec.com/index.php?route=product/product&product_id=289)
- <sup>(5)</sup> 라즈베리 파이4 [(제조사 사양 보기)](https://www.raspberrypi.com/products/raspberry-pi-4-model-b/)

## 특장점
#### 1. 차번 인식 성능
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
  - *차번 인식 결과 데이터의  `ev`항목에 `true` 또는 `false`로 구분합니다.*
  - *단, 영업용 차량 번호판처럼 번호판 규격상 내연기관 차량과 구분되지 않는 경우는 판단이 불가능합니다.*
  <div>
    <img style="margin-right:-5px" width="120" src="img/ex/ev2.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/ev1.jpg" />
  </div>
- ’80, ’90년대 구형 번호판
  - *1996년도 번호판 규격 개정 이전에 사용되던 `처`, `퍼`, `차`, `파`, `추` ~ `후`, `그` ~ `흐` 문자를 지원합니다.*
  - 구형 주한미군 번호판 형식을 지원합니다.
  <div>    
    <img style="margin-right:-5px" width="120" src="img/ex/801.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/802.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/803.jpg" />
    <img style="margin-right:-5px" width="120" src="img/ex/804.jpg" />
    <img style="margin-right:-5px" width="120" src="https://i.namu.wiki/i/wwI1jJQAIZlh_gSD3Vt-2rmuIzYkQ4BNTNTLWv6GU9RMTL01ujgvhxYpFKR0ckzqa-q6_O4L4v0V8AUliVczf7INwNgsbw3DBnDZlkk8aRzGVqkLovKDVfdxkhNYEZqpn4Z90-AeizRDVzFNriHWSQ.webp" />
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
- [프로그래밍 언어별 예제 제공](DevGuide.md#4-%EC%98%88%EC%A0%9C) 
  - `C`, `C++`, `C#`, `Visual Basic`, `Python`, `JavaScript/Node.js`, `Go`, `Pascal/Delphi`, `Perl`, `Ruby`
- [입력 이미지 파일 형식](DevGuide.md#12-anpr_read_file)
  - `bmp`, `jpg`, `png`, `pnm`, `pbm`, `pgm`, `ppm`, `jfif`, `webp`
- [입력 이미지 메모리 버퍼 픽셀 형식](DevGuide.md#13-anpr_read_pixels)
  - `GRAY`, `BGRA`, `RGBA`, `RGB`, `BGR`, `BGR555`, `BGR565`, `HSV`, `YCrCb`, `I420`, `YV12`, `IYUV`, `NV12`, `NV21`
- [인식 결과 출력 형식](DevGuide.md#2-output-format)
  - `text`, `csv`, `json`, `yaml`, `xml`

#### 5. 다양한 라이선스 제공
- 무료 평가판 라이선스
  - 개발 및 데모용으로 시스템당 설치 이후 30일간 무료 사용 기간 제공
- 상용 라이선스
  - 매체별: USB 동글, 또는 소프트웨어 라이선스 중 선택
  - 기능 및 성능별: `IoT`, `기본`, `객체인식`, `프로`, `서버` 중 응용 소프트웨어 요구사항에 따라 선택 가능 ([참고: TS-ANPR 엔진 바이너리](LICENSE.md#2-ts-anpr-엔진-바이너리))

## 다양한 인식 옵션
#### 1. 차량 부착 검사 (Vehicle Mounted)
차체가 보이는 이미지에서 차량에 부착된 번호판인지 구분합니다.
**차량 부착(v)** 옵션을 사용하면 차량에 부착된 번호판만 인식합니다.<br/>
<img width="500" src="img/mounted1.jpg" />

아래 이미지처럼 차량없이 번호판만 있거나 바이크 번호판 등은 무시합니다.<br/>

<img width="500" src="img/mounted2.jpg">
<div style="font-size:0.8em">[이미지 출처: 연합뉴스]</div>
</img>

<br/>

<img width="500" src="img/mounted2-1.jpg">
<div style="font-size:0.8em">[이미지 출처: 바이커즈랩]</div>
</img>

<br/>

번호판만 근접 촬영된 경우는 차량 인식이 안되는 경우가 있는데, 이런 경우 **차량 부착(v)** 옵션을 사용하지 않으면 차량 번호를 인식할 수 있습니다.<br/>
<img width="500" src="img/mounted3.jpg" />

#### 2. 다중 인식 (Multiple Recognition)
**다중 인식(m)** 옵션을 사용하면 이미지에 차량이 여러 대 있으면 모두 인식합니다.<br/>
<img width="800" src="img/multiple1.jpg" />

**다중 인식(m)** 옵션을 사용하지 않으면 여러 대 차량 중 가장 번호판 신뢰도가 높은(잘 보이는) 것 하나만 인식합니다.<br/>
<img width="800" src="img/multiple2.jpg" />


#### 3. 360° 서라운드 인식 (Surround Recognition)
**360° 서라운드 인식(s)** 옵션을 사용하면 전복된 차량 또는 어안 렌즈 카메라로 촬영한 차량 등 이미지 내의 차량이 사방으로 기울어져 있거나 넘어져 있는 경우도 차량 번호를 인식할 수 있습니다.<br/>

<img width="800" src="img/surround1.jpg">
<div style="font-size:0.8em">[이미지 출처: KBS]</div>
</img>

<br/>

<img width="800" src="img/surround2.jpg" />

#### 4. 객체 인식 (Object Detection)
**객체 인식(d)** 옵션을 사용하면 이미지 내의 객체를 인식합니다.
출력된 차량 영역과 응용 프로그램에서 설정한 주차면 영역을 비교하면 만.공차 여부를 판단할 수 있습니다.<br/>

<img width="800" src="img/options/dms.png" />

#### 5. 객체(차량)의 차량 번호 인식 (Read License Plate)
**객체 인식(d)** 과 **차량 번호 인식(r)** 옵션을 함께 사용하면 객체 인식된 차량의 번호까지 인식합니다.<br/>

<img width="800" src="img/options/dmsr.png" />

---


- 응용 프로그램 개발 전 단계의 기본적인 성능 테스트는 [차번 인식 라이브 데모](http://tsnvr.ipdisk.co.kr/)를 이용하실 수 있습니다.
- 응용 프로그램 개발 단계에서는 [응용 프로그램 개발 가이드](DevGuide.md) 와 포함된 프로그래밍 언어별 예제들을 참고하시기 바랍니다.
