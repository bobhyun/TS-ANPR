/**
* The MIT License (MIT)
* Copyright © 2022-2025 TS-Solution Corp.
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to all conditions.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
**/

const fs = require('fs');
const path = require('path');
const os = require('os');
const { PNG } = require('pngjs');
const jpeg = require('jpeg-js');
const { loadTSANPR } = require('./tsanpr');

// Base directory for examples
const examplesBaseDir = path.resolve(__dirname, '../../..');

// Generate engine filename depending on platform
function getEngineFileName() {
  const os_name = process.platform;
  const arch_name = process.arch;

  if (os_name == "win32") {
    if (arch_name == "x64")
      return path.resolve(
        path.join(examplesBaseDir, "bin", "windows-x86_64", "tsanpr.dll")
      );
    else if (arch_name == "ia32")
      return path.resolve(
        path.join(examplesBaseDir, "bin", "windows-x86", "tsanpr.dll")
      );
  } else if (os_name == "linux") {
    if (arch_name == "x64")
      return path.resolve(
        path.join(examplesBaseDir, "bin", "linux-x86_64", "libtsanpr.so")
      );
    else if (arch_name == "arm64")
      return path.resolve(
        path.join(examplesBaseDir, "bin", "linux-aarch64", "libtsanpr.so")
      );
  }

  console.error("Unsupported target platform");
  process.exit(-1);
}

// Helper: get lowercase file extension
function getLowercaseExt(filename) {
  return path.extname(filename).slice(1).toLowerCase();
}

function decodeImageFile(imgfile) {
  if (!fs.existsSync(imgfile)) {
    console.error("Image file does not exist!");
    return;
  }

  try {
    const ext = getLowercaseExt(imgfile);
    if (ext === 'png') {
      const buffer = fs.readFileSync(imgfile);
      const raw = PNG.sync.read(buffer);

      return {    
        data: raw.data,
        width: raw.width,
        height: raw.height,
        channels: 4,
        pixelFormat: "RGBA",
      };

    } else if (ext === 'jpg' || ext === 'jpeg') {
      const buffer = fs.readFileSync(imgfile);
      const raw = jpeg.decode(buffer, { formatAsRGBA: false });

      return {
        data: raw.data,
        width: raw.width,
        height: raw.height,
        channels: 3,
        pixelFormat: "BGR",
      };

    } else {
      throw new Error('Unsupported image format');
    }
  } catch (e) {
    console.error("Image load/decoding failed!", e);
  }
}

function readImageFile(tsanpr, imgfile, outputFormat, options) {
  console.log(`${imgfile} (outputFormat="${outputFormat}", options="${options}") =>`);
  const result = tsanpr.anpr_read_file(imgfile, outputFormat, options);
  console.log(result);
}

function readEncodedImage(tsanpr, imgfile, outputFormat, options) {
  console.log(`${imgfile} (outputFormat="${outputFormat}", options="${options}") =>`);
  if (!fs.existsSync(imgfile)) {
    console.log("File open failed");
    return;
  }
  const encodedImg = fs.readFileSync(imgfile);
  if (!encodedImg || !encodedImg.length) {
    console.log("File read failed");
    return;
  }
  
  const result = tsanpr.anpr_read_pixels(
    encodedImg,
    encodedImg.length,
    0,
    0,
    "encoded",
    outputFormat,
    options
  );
  console.log(result);
}

function readPixelBuffer(tsanpr, imgfile, outputFormat, options) {
  console.log(`${imgfile} (outputFormat="${outputFormat}", options="${options}") =>`);
  
  const info = decodeImageFile(imgfile);
  if (!info) {
    return;
  }
  const { data, width, height, channels, pixelFormat } = info;
  const result = tsanpr.anpr_read_pixels(
    data,
    width,
    height,
    width * channels,
    pixelFormat,
    outputFormat,
    options
  );
  console.log(result);
}

function getImageDir(countryCode) {
  return path.join(examplesBaseDir, 'img', countryCode);
}

function readLicensePlates(tsanpr, countryCode) {

  // NOTICE:
  // anpr_initialize should be called only once after library load.
  // Therefore, it is not possible to change the country code after anpr_initialize has been called.
  // While using the free trial license, you can try all languages.
  // When you purchase a commercial license, you can only use the selected language.
  const initParams = `text;country=${countryCode}`;
  const error = tsanpr.anpr_initialize(initParams);
  if (error && error.length > 0) {
    console.log(`anpr_initialize() failed (error=${error})`);
    return -1;
  }

  const imageDir = getImageDir(countryCode);

  // TODO: Try each function as needed
  const anprFunc = readImageFile;
  // const anprFunc = readEncodedImage;
  // const anprFunc = readPixelBuffer;

  // TODO: Try each output format as needed
  const outputFormat = "text";
  // const outputFormat = "json";
  // const outputFormat = "yaml";
  // const outputFormat = "xml";
  // const outputFormat = "csv";

  anprFunc(tsanpr, path.join(imageDir, "licensePlate.jpg"), outputFormat, "");  // Single license plate recognition (default)
  anprFunc(tsanpr, path.join(imageDir, "multiple.jpg"), outputFormat, "vm");    // Recognize multiple license plates attached to vehicles
  anprFunc(tsanpr, path.join(imageDir, "multiple.jpg"), outputFormat, "vmb");   // Recognize multiple license plates including motorcycles
  anprFunc(tsanpr, path.join(imageDir, "surround.jpg"), outputFormat, "vms");   // Recognize multiple license plates with surround detection
  anprFunc(tsanpr, path.join(imageDir, "surround.jpg"), outputFormat, "dms");   // Recognize multiple surrounding objects (vehicles)
  anprFunc(tsanpr, path.join(imageDir, "surround.jpg"), outputFormat, "dmsr");  // Recognize multiple surrounding objects and license plates
  // Recognize multiple surrounding objects and license plates within RoI
  anprFunc(tsanpr, path.join(imageDir, "surround.jpg"), outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700");

  return 0;
}

// Main entry point
function main() {
  const engineFileName = getEngineFileName();
  const tsanpr = loadTSANPR(engineFileName);

  // TODO: Try each country code as needed
  readLicensePlates(tsanpr, "KR");
  // readLicensePlates(tsanpr, "JP");
  // readLicensePlates(tsanpr, "VN");
}

main();
