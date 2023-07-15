/*
  이 예제는 TS-ANPR 엔진 파일을 다운로드받아 examples/bin/ 디렉토리에 
  압축을 풀어 아래와 같은 디렉토리 구조로 만들어진 상태에서 동작합니다.

  examples
    /bin
      /windows-x86_64
      /windows-x86
      /linux-x86_64
      /linux-aarch64
*/

const path = require("path");
const fs = require("fs");
const ffi = require("ffi-napi");
const ref = require("ref-napi");
const jpeg = require("jpeg-js");

function getLibPath() {
  const os_name = process.platform;
  const arch_name = process.arch;
  console.log(`os_name=${os_name}, arch_name=${arch_name}`);

  if (os_name == "win32") {
    if (arch_name == "x64")
      return path.resolve(
        path.join(__dirname, "..", "..", "bin", "windows-x86_64", "tsanpr.dll")
      );
    else if (arch_name == "ia32")
      return path.resolve(
        path.join(__dirname, "..", "..", "bin", "windows-x86", "tsanpr.dll")
      );
  } else if (os_name == "linux") {
    if (arch_name == "x64")
      return path.resolve(
        path.join(__dirname, "..", "..", "bin", "linux-x86_64", "libtsanpr.so")
      );
    else if (arch_name == "arm64")
      return path.resolve(
        path.join(__dirname, "..", "..", "bin", "linux-aarch64", "libtsanpr.so")
      );
  }

  console.error("Unsupported target platform");
  process.exit(-1);
}

const IMG_PATH = "../../img";
const LIB_PATH = getLibPath();
console.log("LIB_PATH=", LIB_PATH);

const lib = ffi.Library(LIB_PATH, {
  /*
  const char* WINAPI anpr_initialize(const char* mode); // [IN] 라이브러리 동작 방식 설정
  */
  anpr_initialize: ["string", ["string"]],
  /*
  const char* WINAPI anpr_read_file(
    const char* imgFileName,      // [IN] 입력 이미지 파일명
    const char* outputFormat,     // [IN] 출력 데이터 형식
    const char* options);         // [IN] 기능 옵션
  */
  anpr_read_file: ["string", ["string", "string", "string"]],
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
  anpr_read_pixels: [
    "string",
    ["pointer", "int32", "int32", "int32", "string", "string", "string"],
  ],
});

function readFile(imgFile, outputFormat, options) {
  process.stdout.write(
    `${imgFile} (outFormat="${outputFormat}", options="${options}") => `
  );

  // 이미지 파일명 입력으로 차번 인식
  let result = lib.anpr_read_file(imgFile, outputFormat, options);
  console.log(result);
}

function readPixels(imgFile, outputFormat, options) {
  process.stdout.write(
    `${imgFile} (outFormat="${outputFormat}", options="${options}") => `
  );

  // 이미지 파일을 메모리에 로딩
  let jpegData = fs.readFileSync(imgFile);
  let img = jpeg.decode(jpegData, {
    formatAsRGBA: false,
  });

  // 픽셀 버퍼 입력으로 차번 인식
  let result = lib.anpr_read_pixels(
    img.data,
    img.width,
    img.height,
    0,
    "BGR",
    outputFormat,
    options
  );
  console.log(result);
}

function anprDemo1(outputFormat) {
  readFile(path.join(IMG_PATH, "licensePlate.jpg"), outputFormat, "v");
  readFile(path.join(IMG_PATH, "licensePlate.jpg"), outputFormat, "");
  readFile(path.join(IMG_PATH, "multiple.jpg"), outputFormat, "vm");
  readFile(path.join(IMG_PATH, "multiple.jpg"), outputFormat, "");
  readFile(path.join(IMG_PATH, "surround.jpg"), outputFormat, "vms");
  readFile(path.join(IMG_PATH, "surround.jpg"), outputFormat, "");
}

function anprDemo2(outputFormat) {
  readPixels(path.join(IMG_PATH, "licensePlate.jpg"), outputFormat, "v");
  readPixels(path.join(IMG_PATH, "licensePlate.jpg"), outputFormat, "");
  readPixels(path.join(IMG_PATH, "multiple.jpg"), outputFormat, "vm");
  readPixels(path.join(IMG_PATH, "multiple.jpg"), outputFormat, "");
  readPixels(path.join(IMG_PATH, "surround.jpg"), outputFormat, "vms");
  readPixels(path.join(IMG_PATH, "surround.jpg"), outputFormat, "");
}

(function () {
  let error = lib.anpr_initialize("text");
  if (error) {
    console.log(error);
    return -2;
  }

  // 이미지 파일을 입력으로 사용하는 예제
  anprDemo1("text");
  anprDemo1("json");
  anprDemo1("yaml");
  anprDemo1("xml");

  // 픽셀 버퍼를 입력으로 사용하는 예제
  anprDemo2("text");
  anprDemo2("json");
  anprDemo2("yaml");
  anprDemo2("xml");
})();
