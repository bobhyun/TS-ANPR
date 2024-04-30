응용 프로그램 개발 가이드
===

## 목차

- [응용 프로그램 개발 가이드](#응용-프로그램-개발-가이드)
  - [목차](#목차)
  - [1. Entry points](#1-entry-points)
    - [1.1. anpr\_initialize](#11-anpr_initialize)
    - [1.2. anpr\_read\_file](#12-anpr_read_file)
    - [1.3. anpr\_read\_pixels](#13-anpr_read_pixels)
  - [2. Output Format](#2-output-format)
    - [2.1. 차량 번호 인식 결과](#21-차량-번호-인식-결과)
      - [2.1.1. `text`](#211-text)
      - [2.1.2. `csv`](#212-csv)
      - [2.1.3. `json`](#213-json)
      - [2.1.4. `yaml`](#214-yaml)
      - [2.1.5. `xml`](#215-xml)
    - [2.2. 객체 인식 결과](#22-객체-인식-결과)
      - [2.2.1. `csv`, `text`](#221-csv-text)
      - [2.2.2. `json`](#222-json)
      - [2.2.3. `yaml`](#223-yaml)
      - [2.2.4. `xml`](#224-xml)
  - [3. 오류 코드표](#3-오류-코드표)
  - [4. 예제](#4-예제)


## 1. Entry points

모든 함수 원형은 아래와 같습니다.
```cpp
#ifdef WIN32
#define TS_ANPR_ENTRY extern "C" __declspec(dllexport) const char* WINAPI
#else
#define TS_ANPR_ENTRY extern "C" const char* 
#endif
```
**-* 장황해지지 않도록 이하는 `TS_ANPR_ENTRY`로 표기합니다.*

### 1.1. anpr_initialize

라이브러리를 초기화 합니다.
라이브러리를 사용하기 위해 다른 함수보다 먼저 한 번 호출해야 합니다.


```cpp
TS_ANPR_ENTRY anpr_initialize(const char* mode); // [IN] 라이브러리 동작 방식 설정
```

**Parameters**:
- `mode`
  - 라이브러리 동작 방식을 지정하는 목적으로 사용 (기존 `outputFormat`에서 용도 확장)
  - 세미콜론(`;`) 문자로 구분하여 여러 설정을 표현할 수 있음 (예: `json;sync`)
- 지정 가능한 항목
  - `outputFormat`: 
    - 출력 데이터 형식
    - 지원하는 데이터 형식: `text`, `json`, `yaml`, `xml`, `csv` *(기본값: `text`)*
    - `outputFormat`생략하고 간단히 `text`, `json`으로 사용 가능
  - `sync`:
    - 동기 모드로 실행 (쓰레드 lock을 걸고 호출한 순서대로 처리)
    - 미리 생성된 고정 갯수의 쓰레드풀 형태가 아니고 쓰레드가 계속 새로 생성되는 구조의 응용 프로그램에서 호출하는 경우, 아래와 같은 오류 코드 발생시 사용을 고려할 수 있음
      - `103: Too many workers`    라이브러리 호출 쓰레드 수가 한계를 초과한 경우 (최대 256개)
      - `104: Resource exhausted`  더 이상 자원을 할당할 수 없는 경우
    - 복잡한 비동기 쓰레드 관리를 신경쓰지 않아도 되는 반면 쓰레드 락(lock)을 사용하는 방식이므로 성능은 다소 떨어질 수 있음
    - `sync=true` 또는 `sync=false` 로 표현할 수 있으며, 간단히 `sync`만 사용해도 됨 (지정안하면 기본값 `sync=false`로 동작) 
  
**Return value**:
  - 정상 처리된 경우 빈 텍스트 `NULL terminated string (0x00)`을 반환합니다.
  - 오류가 발생한 경우는 `mode`의 `outputFormat`으로 지정한 데이터 형식의 문자열(utf-8 인코딩)로 오류 내용을 반환합니다.


### 1.2. anpr_read_file

이미지 파일에서 차량 번호를 인식합니다.

```cpp
TS_ANPR_ENTRY anpr_read_file(
  const char* imgFileName,  // [IN] 입력 이미지 파일명
  const char* outputFormat, // [IN] 출력 데이터 형식
  const char* options);     // [IN] 기능 옵션
```

**Parameters**:
- `imgFileName`: 
  - 입력 이미지 파일명 *(utf-8 인코딩)*
  - 지원하는 이미지 파일 형식: `bmp`, `jpg`, `png`, `pnm`, `pbm`, `pgm`, `ppm`, `jfif`, `webp`
- `outputFormat`: 
  - 출력 데이터 형식
  - 지원하는 데이터 형식: `text`, `json`, `yaml`, `xml`, `csv` *(기본값: `text`)*
  - 객체 인식 옵션을 사용할 경우 `text`는 `csv`로 출력됩니다.
- `options`: 
  - 아래 문자들을 조합하여 번호 인식 알고리즘의 옵션을 지정합니다.
      | 문자     | 의미                             | 적용 라이선스
      |---------|----------------------------------|---------------
      | `v`     | 번호판 차량 부착 여부 판단        | 전체
      | `m`     | 여러 대의 차량 번호판을 모두 인식 (다중 인식)         | `객체인식`, `프로`, `서버`
      | `s`     | 360° 모든 각도의 차량에서 번호판 인식 (서라운드 인식) | `객체인식`, `프로`, `서버`
      | `d`     | 객체 인식 (만.공차용)               | 전체
      | `r`     | 인식된 객체(차량)의 차량 번호 인식  | 전체
      
  - 라이선스 종류에 따라 기능 및 다중 인식 차량 수가 다르게 적용됩니다. ([참고: TS-ANPR 엔진 바이너리](LICENSE.md#2-ts-anpr-엔진-바이너리))
  

  - 옵션 선택 안내
    아래 플로우차트에서 `시작`에서 `선택완료`까지 조건을 따라 이동하면서 지나간 초록색 원의 문자들을 모아 옵션 값으로 사용하면 됩니다. (문자의 순서는 무관합니다.)
    예) 이동 경로가 `시작` → `v` → `s` → `m` → `선택완료` 이면 `"vsm"`가 옵션 값입니다.
    
    ```mermaid
    flowchart TD
      
      start[/시작/]-->attach{{번호판 차량 부착 여부?}}
      attach-->|상관없음|multi
      attach-->|차량에 부착된 번호판만|v((v)):::opt
      
      v-->angle{{차량 각도?}}
      angle-->|바로 선 차량만|multi{{다중인식?}}
      angle-->|360° 모든 각도|s((s)):::opt
      s-->multi
      multi-->|예|m((m)):::opt
      multi-->|아니오|done[/선택완료/]
      m-->done      

      start-->d((d)):::opt
      d-->angle2{{차량 각도?}}
      angle2-->|바로 선 차량만|multi2{{다중인식?}}
      angle2-->|360° 모든 각도|s2((s)):::opt
      s2-->multi2
      multi2-->|예|m2((m)):::opt
      multi2-->|아니오|read
      m2-->read{{번호 인식?}}
      
      read-->|예|r((r)):::opt
      read-->|아니오|done
      r-->done
     

      subgraph "번호 인식"
        attach
        v
        angle
        multi
        m
        s
      end

      subgraph "객체 인식"
        read
        angle2
        multi2
        s2
        m2
        d
        r
      end

      classDef opt fill:#6F6,stroke:#CCC,stroke-width:4px,padding:10px,font-weight:bold
    ```

  - 번호 인식 옵션 사용 예            
      | options | 의미
      |---------|------------------------------------------------------------
      |         | 모든 번호판 중 하나 인식
      | `v`     | 차량 부착된 번호판 중 하나 인식
      | `m`     | 모든 번호판, 다중 인식
      | `vm`    | 차량 부착된 번호판, 다중 인식
      | `vs`    | 차량 부착된 번호판, 360° 서라운드, 하나 인식
      | `vsm`   | 차량 부착된 번호판, 360° 서라운드, 다중 인식
  
  - 객체 인식 옵션 사용 예
      | options | 의미
      |---------|------------------------------------------------------------
      | `d`     | 단일 객체 인식 (차량 번호 인식 안함)
      | `dr`    | 단일 객체, 차량 번호 인식
      | `ds`    | 단일 객체, 360° 서라운드 인식 (차량 번호 인식 안함)
      | `dsr`   | 단일 객체, 360° 서라운드, 차량 번호 인식
      | `dm`    | 다중 객체 인식 (차량 번호 인식 안함)
      | `dmr`   | 다중 객체, 차량 번호 인식 (차량 번호 인식 안함)
      | `dms`   | 다중 객체, 360° 서라운드 (차량 번호 인식 안함)
      | `dmsr`  | 다중 객체, 360° 서라운드, 차량 번호 인식

**Return value**:
  - `outputFormat`에 지정한 데이터 형식의 문자열(utf-8 인코딩)로 번호 인식 결과를 반환합니다.
  - 사용한 `options`이 번호인식과 객체인식 계열에 따라 `outputFormat` 형식이 이원화되어 있습니다. ([참고: 2. Output Format](#2-output-format))

**Remarks**:
  - `Return value`에 사용되는 문자열 버퍼는 라이브러리 내부에서 관리되며 응용 프로그램에서는 문자열 버퍼를 참조하기만 하면 됩니다.
  - 이 문자열 버퍼는 thread-safe하며 각 thread 별로 다음 호출 전까지 결과 값이 유지됩니다.
  - 참고 사이트
    - https://docs.microsoft.com/ko-kr/windows/win32/medfound/image-stride
    - https://docs.microsoft.com/ko-kr/windows/win32/medfound/video-fourccs

### 1.3. anpr_read_pixels

로딩된 이미지의 메모리 버퍼에서 차량 번호를 인식합니다.

```cpp
TS_ANPR_ENTRY anpr_read_pixels(
  const unsigned char* pixels,  // [IN] 이미지 픽셀 시작 주소
  const unsigned long width,    // [IN] 이미지 가로 픽셀 수
  const unsigned long height,   // [IN] 이미지 세로 픽셀 수
  const unsigned long stride,   // [IN] 이미지 한 라인의 바이트 수
  const char* pixelFormat,      // [IN] 이미지 픽셀 형식 
  const char* outputFormat,     // [IN] 출력 데이터 형식
  const char* options);         // [IN] 기능 옵션
```

**Parameters**:
- `pixels`: 
  - 이미지 픽셀 시작 주소
- `width`: 
  - 이미지 가로 픽셀 수
- `height`: 
  - 이미지 세로 픽셀 수
- `stride`: 
  - 이미지 한 라인의 바이트 수 (`0`이면 padding영역이 없는 것으로 간주하고 자동 계산)
- `pixelFormat`: 
  - 이미지 픽셀 포멧
  - 지원하는 픽셀 포멧: 
    - `GRAY`: 흑백 이미지 (8bpp)
    - `BGRA`: BGRA (32bpp)
    - `RGBA`: RGBA (32bpp)
    - `RGB`: RGB (24bpp)
    - `BGR`: BGR (24bpp)
    - `BGR555`: BGR (16bpp)
    - `BGR565`: BGR (16bpp)
    - `HSV`: HSV (32bpp)
    - `YCrCb`: YUV444 (32bpp)
    - `I420`: YUV420 (12bpp)
    - `YV12`: YUV420 (12bpp)
    - `IYUV`: YUV420 (12bpp) 
    - `NV12`: YUV420 (12bpp)
    - `NV21`: YUV420 (12bpp)
- `outputFormat`: *(`anpr_read_file`과 동일)*
- `options`: *(`anpr_read_file`과 동일)*

**Return value**: *(`anpr_read_file`과 동일)*
**Remarks**: *(`anpr_read_file`과 동일)*


## 2. Output Format

출력 데이터는 `options`에 `d`가 포함되는 객체 인식과 포함되지 않는 차량 번호 인식 두 가지 형식으로 구분됩니다.

### 2.1. 차량 번호 인식 결과

### 2.1.1. `text`
차량 번호 텍스트만 출력합니다. 
번호판이 여러 개인 경우는 줄바꿈 문자 `CR (0x0d)`로 구분합니다.
```text
01가2345
67나8901
```

차량 번호가 인식되지 않은 경우는 빈 텍스트`NULL terminated string (0x00)`를 출력합니다.


오류가 반환되는 경우는 아래와 같은 텍스트 형식으로 출력합니다.
```text
error: (1) Invalid parameters
```

##### 2.1.2. `csv`
차량 번호와 속성을 `csv` 형식으로 출력합니다. 
인식된 차량 번호 당 한 라인 씩으로 구성되며 각 컬럼은 콤마 문자(`,`)로 구분됩니다.
```csv
01가2345,1217,2083,92,175,12.45,0.75,0.83,0.20,ev
67나8901,1108,1317,67,217,12.45,0.76,0.89,0.10,
```

각 컬럼의 의미는 다음과 같습니다.
| 컬럼  | 의미                      | 비고
|-----:|---------------------------|------------------------
| 1    | 차량번호                   | `text`
| 2    | 번호판 x픽셀 좌표           | `area.x`
| 3    | 번호판 y픽셀 좌표           | `area.y`
| 4    | 번호판 폭                  | `area.width`
| 5    | 번호판 높이                | `area.height`
| 6    | 번호판 각도                | `area.angle`
| 7    | 문자 인식 신뢰도            | `conf.ocr`
| 8    | 번호판 인식 신뢰도          | `conf.plate`
| 9    | 문자 인식 소요 시간 (초)    | `elapsed`
| 10<sup>(1)</sup>   | 친환경 전기자동차 여부      | `attr.ev`
- <sup>(1)</sup> 친환경 전기자동차이면 `ev`가 출력되고 아니면 공란으로 표시됨

차량 번호가 인식되지 않은 경우는 빈 텍스트`NULL terminated string (0x00)`를 출력합니다.

오류가 반환되는 경우는 아래와 같은 텍스트 형식으로 출력합니다.
```csv
error,1,Invalid parameters
```


### 2.1.3. `json`
차량 번호와 속성을 `json` 형식으로 출력합니다.
```jsx
[
  {                         // 첫번째 번호판  
    "text": "01가2345",     // 차량 번호
    "area": {               // 번호판 영역 (픽셀 단위)
      "x": 1217,            // 좌측 상단 x 좌표
      "y": 2083,            // 좌측 상단 y 좌표
      "width": 92,          // 폭
      "height": 175,        // 높이
      "angle": 12.45        // 기울기 (도)
    },
    "attrs": {              // 번호판 속성
      "ev": true            // 친환경 전기자동차 여부
    },
    "ev": true,             // deprecated (attrs.ev로 옮김, 추후 버전에서 삭제 예정)
    "conf": {               // 신뢰도 (범위: 0 ~ 1)
      "ocr": 0.75,          // 문자 인식 신뢰도
      "plate": 0.84         // 번호판 인식 신뢰도
    },
    "elapsed": 0.27         // 소요 시간 (초 단위)
  },
  {                         // 두번째 번호판
    "text": "67나8901",
    "area": {
      "x": 1108,
      "y": 1317,
      "width": 67,
      "height": 217,
      "angle": 12.45
    },
    "attrs": {
      "ev": false
    },
    "ev": false,
    "conf": {
      "ocr": 0.76,
      "plate": 0.89
    },
    "elapsed": 0.14
  }
]
```

차량 번호가 인식되지 않은 경우는 아래와 같이 빈 데이터를 출력합니다.
```jsx
[]
```

오류가 반환되는 경우는 아래와 같은 `json` 형식으로 출력합니다.
```jsx
{
  "error": {
    "code": 1,
    "message": "Invalid parameters"
  }
}
```

### 2.1.4. `yaml`
차량 번호와 속성을 `yaml` 형식으로 출력합니다.
````yaml
- text: 01가2345        # 첫번째 번호판, 차량 번호 
  area:                 # 번호판 영역 (픽셀 단위)
    x: 1217             # 좌측 상단 x 좌표
    y: 2083             # 좌측 상단 y 좌표
    width: 92           # 폭
    height: 175         # 높이
    angle: 12.45        # 기울기 (도)
  conf:                 # 신뢰도 (범위: 0 ~ 1)
    ocr: 0.75           # 문자 인식 신뢰도
    plate: 0.83         # 번호판 인식 신뢰도
  attrs:                # 번호판 속성
    ev: true            # 친환경 전기자동차 여부
  ev: true              # deprecated (attrs.ev로 옮김, 추후 버전에서 삭제 예정)
  elapsed: 0.20         # 소요 시간 (초)
- text: 67나8901        # 두번째 번호판
  area:
    x: 1108
    y: 1317
    width: 67
    height: 217
    angle: 12.45
  conf:
    ocr: 0.76
    plate: 0.89
  ev: false
  elapsed: 0.10
````

차량 번호가 인식되지 않은 경우는 아래와 같이 빈 데이터를 출력합니다.
```yaml
```

오류가 반환되는 경우는 아래와 같은 `yaml` 형식으로 출력합니다.
```yaml
error
  code: 1
  message: Invalid parameters
```

### 2.1.5. `xml`
차량 번호와 속성을 `xml` 형식으로 출력합니다.
```xml
<?xml version="1.0" encoding="utf-8"?>
<data>
  <!-- 첫번째 번호판 
    text: 차량 번호
    ev: deprecated (attrs.ev로 옮김, 추후 버전에서 삭제 예정)
    elapsed: 소요 시간 (초)
  -->
  <license-plate text="01가2345" ev="true" elapsed="0.20">
    <!-- 번호판 속성
      ev: 친환경 전기자동차 여부 
    -->    
    <attrs ev="true"/>
    <!-- 번호판 영역 (픽셀 단위)
      x: 좌측 상단 x 좌표
      y:  좌측 상단 y 좌표
      width: 폭
      height: 높이
      angle: 기울기 (도)
    -->
    <area x="1217" y="2083" width="92" height="175" angle="12.45"/>
    <!-- 신뢰도 (범위: 0 ~ 1)
      ocr: 문자 인식 신뢰도
      plate: 번호판 인식 신뢰도
    -->
    <conf ocr="0.75" plate="0.83"/>
  </license-plate>
  <!-- 두번째 번호판 -->
  <license-plate text="67나8901" ev="false" elapsed="0.11">
    <attrs ev="false"/>
    <area x="1108" y="1317" width="67" height="217"/>
    <conf ocr="0.76" plate="0.89"/>
  </license-plate>
</data>
```

차량 번호가 인식되지 않은 경우는 아래와 같이 빈 데이터를 출력합니다.
```xml
<?xml version="1.0" encoding="utf-8"?>
<data />
```

오류가 반환되는 경우는 아래와 같은 `xml` 형식으로 출력합니다.
```xml
<?xml version="1.0" encoding="utf-8"?>
<error code="1" message="Invalid parameters" />
```


#### 2.2. 객체 인식 결과
이미지에서 미리 학습한 객체를 찾아냅니다.
차별화된 특징은 360° 어안 렌즈 카메라로 촬영된 원본 이미지에서 외곡된 형태의 객체를 인식합니다.
현재 지원하는 객체 목록은 아래와 같습니다.
| class        | 이름                             
|--------------|----------------------------------
| `car`        | 차량
| `motorcycle` | 오토바이

##### 2.2.1. `csv`, `text`
객체 인식 결과를 cvs 형식의 텍스트로 출력합니다.
출력 형식을 `text`로 지정한 경우도 csv 형식으로 대체되어 출력됩니다.

인식된 객체 당 한 라인 씩으로 구성되며 각 컬럼은 콤마 문자(`,`)로 구분됩니다.
```csv
car,2171,2281,396,521,0.9679,0.2886,51조8969,2420,2295,110,81,147.5933,0.9005,0.7864,0.3913,ev
car,264,2266,433,543,0.9706,0.2886,41노7656,315,2281,103,81,211.3135,0.9160,0.8299,0.4189,
car,777,0,579,403,0.9716,0.2886 // 번호판 정보가 없는 경우
```
각 컬럼의 의미는 다음과 같습니다.
| 컬럼  | 의미                              | 비고
|-----:|-----------------------------------|------------------------
| 1    | 종류                               | `class`
| 2    | x픽셀 좌표                         | `area.x`
| 3    | y픽셀 좌표                         | `area.y`
| 4    | 폭                                 | `area.width`
| 5    | 높이                               | `area.height`
| 6    | 신뢰도                             | `conf`
| 7    | 소요 시간(초)                      | `elapsed`
| 8<sup>(1)</sup>    | 차량번호             | `licensePlate.text`
| 9    | 번호판 x픽셀 좌표                  | `licensePlate.area.x`
| 10   | 번호판 y픽셀 좌표                  | `licensePlate.area.y`
| 11   | 번호판 폭                          | `licensePlate.area.width`
| 12   | 번호판 높이                        | `licensePlate.area.height`
| 13   | 번호판 각도                        | `licensePlate.area.angle`
| 14   | 문자 인식 신뢰도                   | `licensePlate.conf.ocr`
| 15   | 번호판 인식 신뢰도                  | `licensePlate.conf.plate`
| 16   | 문자 인식 소요 시간 (초)            | `licensePlate.elapsed`
| 17<sup>(2)</sup> | 친환경 전기자동차 여부  | `licensePlate.attrs.ev`

- <sup>(1)</sup> 8번 컬럼부터는 `r` 옵션을 사용하여 차량 번호가 인식된 경우만 출력되며 아니면 이하 컬럼이 모두 생략됨
- <sup>(2)</sup> 17번 컬럼은 친환경 전기자동차로 인식된 경우 `ev`로 출력되며 아니면 공란으로 표시됨

객체가 인식되지 않은 경우는 빈 텍스트`NULL terminated string (0x00)`를 출력합니다.

오류가 반환되는 경우는 아래와 같은 텍스트 형식으로 출력합니다.
```csv
error,1,Invalid parameters
```

##### 2.2.2. `json`
객체 인식 결과를 `json` 형식으로 출력합니다.
```jsx
 [
  {                           // 첫번째 객체
    "class": "car",           // 객체 종류
    "area": {                 // 객체 영역 (픽셀 단위)
      "x": 2171,              // 좌측 상단 x 좌표
      "y": 2281,              // 좌측 상단 y 좌표
      "width": 396,           // 폭
      "height": 521           // 높이
    },
    "conf": 0.9679,           // 객체 인식 신뢰도 (범위: 0 ~ 1)
    "elapsed": 0.2513,        // 소요 시간 (초)
    "licensePlate": [         // 번호판
      {
        "text": "51조8969"    // 차량 번호
        "area": {             // 번호판 영역 (픽셀 단위)
          "x": 2420,          // 좌측 상단 x 좌표
          "y": 2295           // 좌측 상단 y 좌표
          "width": 110,       // 폭
          "height": 81,       // 높이
          "angle": 147.5933   // 기울기 (도)
        },
        "attrs": {            // 번호판 속성
          "ev": true          // 친환경 전기자동차 여부
        },
        "conf": {             // 신뢰도 (범위: 0 ~ 1)
          "ocr": 0.9005,      // 문자 인식 신뢰도
          "plate": 0.7864     // 번호판 인식 신뢰도
        },
        "elapsed": 0.3525,    // 소요 시간 (초)
      }
    ]
  },
  {                           // 두번째 객체    
    "class": "car",
    "area": {      
      "x": 264,
      "y": 2266,
      "width": 433,
      "height": 543
    },
    "conf": 0.9706,
    "elapsed": 0.2513,
    "licensePlate": [
      {
        "text": "41노7656"
        "area": {
          "x": 315,
          "y": 2281,
          "width": 103,
          "height": 81,
          "angle": 211.3135
        },
        "attrs": {
          "ev": false
        },
        "conf": {
          "ocr": 0.916,
          "plate": 0.8299
        },
        "elapsed": 0.4402
      }
    ]
  },
  {                           // 세번째 객체 (번호판 정보가 없는 경우)
    "class": "car",
    "area": {
      "x": 777,
      "y": 0
      "height": 403,
      "width": 579,
    },
    "conf": 0.9716,
    "elapsed": 0.2513
  }
]
```

객체가 인식되지 않은 경우는 아래와 같이 빈 데이터를 출력합니다.
```jsx
[]
```

오류가 반환되는 경우는 아래와 같은 `json` 형식으로 출력합니다.
```jsx
{
  "error": {
    "code": 1,
    "message": "Invalid parameters"
  }
}
```

##### 2.2.3. `yaml`
객체 인식 결과를 `yaml` 형식으로 출력합니다.
````yaml
- class: car            # 첫번째 객체, 객체 종류
  area:                 # 객체 영역 (픽셀 단위)
    x: 2171             # 좌측 상단 x 좌표
    y: 2281             # 좌측 상단 y 좌표
    width: 396          # 폭
    height: 521         # 높이
  conf: 0.9678          # 객체 인식 신뢰도 (범위: 0 ~ 1)
  elapsed: 0.3190       # 소요 시간 (초)
  licensePlate:         # 번호판
    - text: 51조8969    # 차량 번호
      area:             # 번호판 영역 (픽셀 단위)
        x: 2420         # 좌측 상단 x 좌표
        y: 2295         # 좌측 상단 y 좌표
        width: 110      # 폭
        height: 81      # 높이
        angle: 147.5933 # 기울기 (도)
      attrs:            # 번호판 속성
        ev: true        # 친환경 전기자동차 여부
      conf:             # 신뢰도 (범위: 0 ~ 1)
        ocr: 0.9005     # 문자 인식 신뢰도
        plate: 0.7864   # 번호판 인식 신뢰도
      elapsed: 0.3226   # 소요 시간 (초)
- class: car            # 두번째 객체
  area:
    x: 264
    y: 2266
    width: 433
    height: 543
  conf: 0.9706
  elapsed: 0.3191
  licensePlate:
    - text: 41노7656
      area:
        x: 315
        y: 2281
        width: 103
        height: 81
        angle: 211.3135
      conf:
        ocr: 0.916
        plate: 0.8299
      attrs:
        ev: false
      elapsed: 0.5527
- class: car            # 세번째 객체 (번호판 정보가 없는 경우)
  area:
    x: 777
    y: 0
    width: 579
    height: 403
  conf: 0.9716
  elapsed: 0.3191
````

객체가 인식되지 않은 경우는 아래와 같이 빈 데이터를 출력합니다.
```yaml
```

오류가 반환되는 경우는 아래와 같은 `yaml` 형식으로 출력합니다.
```yaml
error
  code: 1
  message: Invalid parameters
```

##### 2.2.4. `xml`
객체 인식 결과를 `xml` 형식으로 출력합니다.
```xml
<?xml version="1.0" encoding="utf-8"?>
<data>
  <!-- 첫번째 객체
    class: 객체 종류
    conf: 객체 인식 신뢰도 (범위: 0 ~ 1)
    elapsed: 소요 시간 (초)
  -->
  <object class="car" conf="0.9679" elapsed="0.3287">
    <!-- 객체 영역 (픽셀 단위)
      x: 좌측 상단 x 좌표
      y: 좌측 상단 y 좌표
      width: 폭
      height: 높이
    -->
    <area x="2171" y="2281" width="396" height="521"/>

    <!-- 번호판
      text: 차량 번호
      elapsed: 소요 시간 (초)
    -->
    <license-plate text="51조8969" elapsed="0.3961">
    
      <!-- 번호판 속성
        ev: 친환경 전기자동차 여부
      -->
      <attrs ev="true"/>
      
      <!-- 번호판 영역 (픽셀 단위)
        x: 좌측 상단 x 좌표
        y: 좌측 상단 y 좌표
        width: 폭
        height: 높이
        angle: 기울기 (도)
      -->
      <area x="2420" y="2295" width="110" height="81" angle="147.5933"/>

      <!-- 신뢰도 (범위: 0 ~ 1)
        ocr: 문자 인식 신뢰도
        plate: 번호판 인식 신뢰도
      -->
      <conf ocr="0.9005" plate="0.7864"/>
    </license-plate>
  </object>

  <!-- 두번째 객체 -->
  <object class="car" conf="0.9706" elapsed="0.3287">
    <area x="264" y="2266" width="433" height="543"/>
    <license-plate text="41노7656" elapsed="0.4364">
      <attrs ev="false"/>
      <area x="315" y="2281" width="103" height="81" angle="211.3135"/>
      <conf ocr="0.9160" plate="0.8299"/>
    </license-plate>
  </object>

  <!-- 세번째 객체 (번호판 정보가 없는 경우) -->
  <object class="car" conf="0.9716" elapsed="0.3287">
    <area x="777" y="0" width="579" height="403"/>
  </object>
</data>
```

객체가 인식되지 않은 경우는 아래와 같이 빈 데이터를 출력합니다.
```xml
<?xml version="1.0" encoding="utf-8"?>
<data />
```

오류가 반환되는 경우는 아래와 같은 `xml` 형식으로 출력합니다.
```xml
<?xml version="1.0" encoding="utf-8"?>
<error code="1" message="Invalid parameters" />
```


## 3. 오류 코드표
전체 오류 목록은 아래 표와 같습니다.
  
  |   code | message                    | 설명
  |-------:|----------------------------|--------------------------------
  |    `1` | `Invalid parameters`       | 함수 호출 인자가 잘못된 경우
  |    `2` | `File not found`           | 입력 이미지 파일이 존재하지 않는 경우
  |    `3` | `Invalid image`            | 입력 이미지 메모리가 형식에 맞지 않는 경우
  |    `4` | `Unsupported image format` | 입력 이미지가 지원되지 않는 형식인 경우 
  |  `100` | `License expired`          | 라이선스가 만료된 경우
  |  `101` | `Corrupted library`        | 라이브러리 구성 파일 중 일부가 없거나 손상된 경우
  |  `102` | `Not initialized`          | 엔진이 초기화되지 않은 상태
  |  `103` | `Too many workers`         | 라이브러리 호출 쓰레드 수가 한계를 초과한 경우 (최대 256개)
  |  `104` | `Resource exhausted`       | 더 이상 자원을 할당할 수 없는 경우
  |  `105` | `License not installed`    | 라이선스가 설치되지 않은 상태 (리눅스에서 무료 평가판 라이센스가 설치되지 않은 경우 발생함)
  |  `106` | `USB dongle I/O error`     | USB 라이선스 동글 읽기 실패시 발생
  |  `107` | `License required`				  | 해당 기능을 사용하기 위한 라이선스가 없음
  |  `108` | `Unsupported platform`     | 지원되지 않는 환경에서 실행시 발생
  |  `200` | `Unknown`                  | 기타 정의되지 않은 오류
  
## 4. 예제

- 디렉토리 구성
    ```
    /examples
      /bin                  # 각 플랫폼별 ANPR 엔진
        /windows-x86_64     # x86 64비트 윈도우즈용
        /windows-x86        # x86 32비트 윈도우즈용
        /linux-x86_64       # x86 64비트 리눅스용
        /linux-aarch64      # ARM 64비트 리눅스용
      /img                  # 테스트용 샘플 이미지
      /cpp                  # C++ 예제
      /csharp               # C#  예제
      /vb                   # Visual Basic 예제
      /javascript/nodejs    # JavaScript/Node.js 예제
      /python               # Python 예제
      /go                   # Golang 예제
      /pascal/delphi        # Pascal/Delphi 예제
      /perl                 # Perl 예제
      /ruby                 # Ruby 예제
    ```

- *TS-ANPR 엔진 디렉토리를 `/examples/bin` 디렉토리에 복사해 넣고 예제를 실행하면 됩니다.*

|      언어     |     호출방식     |  예제
|:-------------:|:---------------:|:------------------------------------------
| C/C++         | Importlib       | [examples/cpp/anprCpp1](https://github.com/bobhyun/TS-ANPR/tree/main/examples/cpp/anprCpp1)
| (상동)          | LoadLibrary     | [examples/cpp/anprCpp2](https://github.com/bobhyun/TS-ANPR/tree/main/examples/cpp/anprCpp2)
| C#            | .Net            | [examples/csharp/anprCsharpDotnet1](https://github.com/bobhyun/TS-ANPR/tree/main/examples/csharp/anprCsharpDotnet1)
| Visual Basic  | .Net            | [examples/vb/anprVbDotnet1](https://github.com/bobhyun/TS-ANPR/tree/main/examples/vb/anprVbDotnet1)
| Python        | ctypes          | [examples/python](https://github.com/bobhyun/TS-ANPR/tree/main/examples/python)
| JavaScript    | Node.js, ffi    | [examples/javascript/nodejs](https://github.com/bobhyun/TS-ANPR/tree/main/examples/javascript/nodejs)
| Go            | C, syscall      | [examples/go](https://github.com/bobhyun/TS-ANPR/tree/main/examples/go)
| Pascal        | Delphi          | [examples/pascal/delphi](https://github.com/bobhyun/TS-ANPR/tree/main/examples/pascal/delphi)
| Perl          | Win32::API      | [examples/perl](https://github.com/bobhyun/TS-ANPR/tree/main/examples/perl)
| Ruby          | ffi             | [examples/ruby](https://github.com/bobhyun/TS-ANPR/tree/main/examples/ruby)