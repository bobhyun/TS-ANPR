/*
  이 예제는 TS-ANPR 엔진 파일을 다운로드받아 example/bin/ 디렉토리에 
  압축을 풀어 아래와 같은 디렉토리 구조로 만들어진 상태에서 동작합니다.

  example
    /bin
      /windows-x86_64
      /windows-x86
      /linux-x86_64
      /linux-aarch64
      /eon
*/

const path = require('path')
const fs = require('fs')
const ffi = require('ffi-napi')
const ref = require('ref-napi')
const jpeg = require('jpeg-js')

// nodejs에서 사용하는 이름이 library path와 일부 달라서 맞춰줌
let _OS = process.platform
if (_OS == 'win32')
  _OS = 'windows'
let _ARCH = process.arch
switch (_ARCH) {
  case 'x64': _ARCH = 'x86_64'; break
  case 'ia32': _ARCH = 'x86'; break
  case 'arm64': _ARCH = 'aarch64'; break
}

const IMG_PATH = '../../img'
const LIB_NAME = (_OS === 'windows') ? 'tsanpr.dll' : 'libtsanpr.so'
const DLL_PATH = path.resolve(path.join(__dirname, '..', '..', 'bin', `${_OS}-${_ARCH}`, LIB_NAME))
console.log('DLL_PATH=', DLL_PATH)

const dll = ffi.Library(DLL_PATH, {
  /*
  const char* WINAPI anpr_initialize(const char* outputFormat); // [IN] 오류 발생시 출력 데이터 형식
  */
  anpr_initialize: ['string', ['string']],
  /*
  const char* WINAPI anpr_read_file(
    const char* imgFileName,      // [IN] 입력 이미지 파일명
    const char* outputFormat,     // [IN] 출력 데이터 형식
    const char* options);         // [IN] 기능 옵션
  */
  anpr_read_file: ['string', ['string', 'string', 'string']],
  /*
  const char* WINAPI anpr_read_pixels(
    const unsigned char* pixels,  // [IN] 이미지 픽셀 시작 주소
    const unsigned long width,    // [IN] 이미지 가로 픽셀 수
    const unsigned long height,   // [IN] 이미지 세로 픽셀 수
    const unsigned long stride,   // [IN] 이미지 한 라인의 바이트 수
    const char* pixelFormat,      // [IN] 이미지 픽셀 형식 
    const char* outputFormat,     // [IN] 출력 데이터 형식
    const char* options);         // [IN] 기능 옵션
  */
  anpr_read_pixels: ['string', ['pointer', 'int32', 'int32', 'int32', 'string', 'string', 'string']],
})


function readFile(imgFile, outputFormat, options) {
  process.stdout.write(`${imgFile} (outFormat="${outputFormat}", options="${options}") => `)
  let result = dll.anpr_read_file(imgFile, outputFormat, options)
  console.log(result)
}

function readPixels(imgFile, outputFormat, options) {
  process.stdout.write(`${imgFile} (outFormat="${outputFormat}", options="${options}") => `)

  let jpegData = fs.readFileSync(imgFile)
  let img = jpeg.decode(jpegData, {
    formatAsRGBA: false
  })

  let result = dll.anpr_read_pixels(img.data, img.width, img.height, 0, 'BGR', outputFormat, options)
  console.log(result)
}

function anprDemo1(outputFormat) {
  readFile(path.join(IMG_PATH, 'licensePlate.jpg'), outputFormat, 'v')
  readFile(path.join(IMG_PATH, 'licensePlate.jpg'), outputFormat, '')
  readFile(path.join(IMG_PATH, 'multiple.jpg'), outputFormat, 'vm')
  readFile(path.join(IMG_PATH, 'multiple.jpg'), outputFormat, '')
  readFile(path.join(IMG_PATH, 'surround.jpg'), outputFormat, 'vms')
  readFile(path.join(IMG_PATH, 'surround.jpg'), outputFormat, '')
}

function anprDemo2(outputFormat) {
  readPixels(path.join(IMG_PATH, 'licensePlate.jpg'), outputFormat, 'v')
  readPixels(path.join(IMG_PATH, 'licensePlate.jpg'), outputFormat, '')
  readPixels(path.join(IMG_PATH, 'multiple.jpg'), outputFormat, 'vm')
  readPixels(path.join(IMG_PATH, 'multiple.jpg'), outputFormat, '')
  readPixels(path.join(IMG_PATH, 'surround.jpg'), outputFormat, 'vms')
  readPixels(path.join(IMG_PATH, 'surround.jpg'), outputFormat, '')
}

(function () {

  let error = dll.anpr_initialize('text')
  if (error) {
    console.log(error)
    return -2
  }

  anprDemo1('text')
  anprDemo1('json')
  anprDemo1('yaml')
  anprDemo1('xml')
  anprDemo1('csv')

  anprDemo2('text')
  anprDemo2('json')
  anprDemo2('yaml')
  anprDemo2('xml')
  anprDemo2('csv')
})()
