const path = require('path')
const fs = require('fs')
const ffi = require('ffi-napi')
const ref = require('ref-napi')
const jpeg = require('jpeg-js')

const IMG_PATH = "..\\..\\img\\"
const DLL_PATH = path.resolve(path.join(__dirname, '..', '..', 'bin', process.arch, 'tsanpr.dll'))
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
  readFile(IMG_PATH + 'licensePlate.jpg', outputFormat, 'v')
  readFile(IMG_PATH + 'licensePlate.jpg', outputFormat, '')
  readFile(IMG_PATH + 'multiple.jpg', outputFormat, 'vm')
  readFile(IMG_PATH + 'multiple.jpg', outputFormat, '')
  readFile(IMG_PATH + 'surround.jpg', outputFormat, 'vms')
  readFile(IMG_PATH + 'surround.jpg', outputFormat, '')
}

function anprDemo2(outputFormat) {
  readPixels(IMG_PATH + 'licensePlate.jpg', outputFormat, 'v')
  readPixels(IMG_PATH + 'licensePlate.jpg', outputFormat, '')
  readPixels(IMG_PATH + 'multiple.jpg', outputFormat, 'vm')
  readPixels(IMG_PATH + 'multiple.jpg', outputFormat, '')
  readPixels(IMG_PATH + 'surround.jpg', outputFormat, 'vms')
  readPixels(IMG_PATH + 'surround.jpg', outputFormat, '')
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
