TS-ANPR
===

TS-ANPR은 딥러닝 기반의 대한민국 차량 번호 인식 엔진입니다.
#### [차번 인식 데모](http://tsnvr.ipdisk.co.kr/) 

#### [최신 엔진 다운로드](https://github.com/bobhyun/TS-ANPR/releases/)

#### [응용 프로그램 개발 가이드](https://github.com/bobhyun/TS-ANPR/blob/main/DevGuide.md) 

- [Entry points](https://github.com/bobhyun/TS-ANPR/blob/main/DevGuide.md#1-entry-points)
- [pdf 문서 다운로드](https://github.com/bobhyun/TS-ANPR/blob/main/doc/TS-ANPR-Manual.pdf)

#### [설치 방법](https://github.com/bobhyun/TS-ANPR/blob/main/Usage.md)


## 최신 버전 정보
#### v1.4.0 출시🎉 (2023-5-9)
- **windows-x86 `v1.4.1` patch 안내**
  - windows-x86 dll (32-bit) `v1.4.0` dll의 entry point naming 오류로 인해 동작하지 않는 문제 수정 `v1.4.1`
  - 나머지 windows-x86_64, linux 파일들은 문제가 없어 기존 버전 유지 `v1.4.0`
  - 이에 따라 수정된 windows-x86 dll (32-bit) `v1.4.1`이 포함된 `ts-anpr-v1.4.1-all.zip`, `ts-anpr-v1.4.1-windows-x86.zip` 파일만 변경됨

- 리눅스🐧(`linux-x86_64`, `linux-aarch64`) 지원 *(라이브러리 인터페이스는 기존 윈도우즈와 동일)*
- 인식 속도 향상 (v1.2.0 대비 약 2.5배 빠름🚀)
- `Server`, `IoT` 라이선스 추가
- 원거리의 여러 차량을 인식시 일부 차량 번호가 누락되는 현상 개선
  ![](img/4vehicles.jpg)

## 특장점
#### 1. 문자 인식 능력
- 번호판 각도
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
  
#### 2. 각종 번호판 지원
- 360도 어안 카메라 이미지
- 덤프트럭, 중장비 번호판
- 특수 번호판 (임시, 외교, 군용)
- 친환경 전기차 번호판 *(인식 결과 데이터에 `ev`항목이 `true / false`로 구분)*
- ’80, ’90년대 구형 번호판
#### 3. 주요 운영체제 / CPU 아키텍처 지원
- 윈도우즈
  - 인텔 계열 64비트(`windows-x86_64`), 32비트(`windows-x84`)
  - 윈도우즈 7 이상 호환
- 리눅스
  - 인텔 계열 64비트(`linux-x86_64`), 
  - ARM 계열 64비트(`linux-aarch64`)
  - 배포판에 관계없이 `glibc 2.27` 이상 호환

#### 4. 다양한 개발 환경 지원
- 특정 프로그래밍 언어에 종속되지 않는 범용 라이브러리 인터페이스
- [프로그래밍 언어별 예제 제공](https://github.com/bobhyun/TS-ANPR/blob/main/DevGuide.md#4-%EC%98%88%EC%A0%9C) (C, C++, C#, Visual Basic, Python, JavaScript/Node.js, Go, Pascal/Delphi, Perl, Ruby)
- [입력 이미지 파일 형식](https://github.com/bobhyun/TS-ANPR/blob/main/DevGuide.md#12-anpr_read_file) (bmp, jpg, png, pnm, pbm, pgm, ppm, jfif, webp)
- [입력 이미지 메모리 버퍼 픽셀 형식](https://github.com/bobhyun/TS-ANPR/blob/main/DevGuide.md#13-anpr_read_pixels) (GRAY, BGRA, RGBA, RGB, BGR, BGR555, BGR565, HSV, YCrCb, I420, YV12, IYUV, NV12, NV21)
- [인식 결과 출력 형식](https://github.com/bobhyun/TS-ANPR/blob/main/DevGuide.md#2-output-format) (text, json, yaml, xml)

#### 5. 다양한 라이선스 제공
- 무료 평가판 라이선스
  - 개발 및 데모용으로 시스템당 설치 이후 30일간 무료 사용 기간 제공
- 상용 라이선스
  - 매체별: USB 동글, 또는 소프트웨어 라이선스 중 선택
  - 기능 및 성능별: `IoT`, `Basic`, `Pro`, `Server` 중 응용 소프트웨어 요구사항에 따라 선택  

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
- 응용 프로그램 개발 단계에서는 [응용 프로그램 개발 가이드](https://github.com/bobhyun/TS-ANPR/blob/main/DevGuide.md) 와 포함된 프로그래밍 언어별 예제들을 참고하시기 바랍니다.
- 개발 관련 질문이나 요청 사항들은 [Issues](https://github.com/bobhyun/TS-ANPR/issues)에 등록해 주시면 적극적으로 지원하겠습니다.


<br/>

- 개발 문의: bobhyun@gmail.com
- 구매 문의: skju3922@naver.com 
- 📞 전화: <a href="tel:02-6084-3920">02-6084-3920</a>
  
<br/>

