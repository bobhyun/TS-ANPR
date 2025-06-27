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

import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { PNG } from 'pngjs';
import * as jpeg from 'jpeg-js';
import { loadTSANPR, TSANPR } from './tsanpr';

// Base directory for example images and binaries
const examplesBaseDir = path.resolve(__dirname, '../../..');

// Generate engine filename depending on platform and architecture
function getEngineFileName(): string {
  const os_name = process.platform;
  const arch_name = process.arch;
  if (os_name === "win32") {
    if (arch_name === "x64")
      return path.resolve(path.join(examplesBaseDir, "bin", "windows-x86_64", "tsanpr.dll"));
    else if (arch_name === "ia32")
      return path.resolve(path.join(examplesBaseDir, "bin", "windows-x86", "tsanpr.dll"));
  } else if (os_name === "linux") {
    if (arch_name === "x64")
      return path.resolve(path.join(examplesBaseDir, "bin", "linux-x86_64", "libtsanpr.so"));
    else if (arch_name === "arm64")
      return path.resolve(path.join(examplesBaseDir, "bin", "linux-aarch64", "libtsanpr.so"));
  }
  console.error("Unsupported target platform");
  process.exit(-1);
}

// Helper: get lowercase file extension
function getLowercaseExt(filename: string): string {
  return path.extname(filename).slice(1).toLowerCase();
}

// Decode image file and determine pixel format and channels
function decodeImageFile(imgfile: string): { data: Buffer, width: number, height: number, channels: number, pixelFormat: string } | undefined {
  if (!fs.existsSync(imgfile)) {
    console.error("Image file does not exist!");
    return;
  }
  try {
    const ext = getLowercaseExt(imgfile);
    if (ext === 'png') {
      const buffer = fs.readFileSync(imgfile);
      const raw = PNG.sync.read(buffer);
      // pngjs returns always RGBA (4 channels)
      return {
        data: raw.data as Buffer,
        width: raw.width,
        height: raw.height,
        channels: 4,
        pixelFormat: "RGBA",
      };
    } else if (ext === 'jpg' || ext === 'jpeg') {
      const buffer = fs.readFileSync(imgfile);
      const raw = jpeg.decode(buffer, { formatAsRGBA: false });
      // jpeg-js returns always 3 channels (BGR)
      return {
        data: raw.data as Buffer,
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
    return;
  }
}

// Call engine with image file path
function readImageFile(tsanpr: TSANPR, imgfile: string, outputFormat: string, options: string): void {
  console.log(`${imgfile} (outputFormat="${outputFormat}", options="${options}") =>`);
  const result = tsanpr.anpr_read_file(imgfile, outputFormat, options);
  console.log(result);
}

// Call engine with encoded image buffer
function readEncodedImage(tsanpr: TSANPR, imgfile: string, outputFormat: string, options: string): void {
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

// Call engine with decoded pixel buffer
function readPixelBuffer(tsanpr: TSANPR, imgfile: string, outputFormat: string, options: string): void {
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

// Get image directory for a given country code
function getImageDir(countryCode: string): string {
  return path.join(examplesBaseDir, 'img', countryCode);
}

// Recognize license plates for a given country code
function readLicensePlates(tsanpr: TSANPR, countryCode: string): number {
  // NOTE:
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

  // Choose which function to use for recognition
  // const anprFunc = readImageFile;
  // const anprFunc = readEncodedImage;
  const anprFunc = readPixelBuffer;

  // Choose output format
  const outputFormat = "text";
  // const outputFormat = "json";
  // const outputFormat = "yaml";
  // const outputFormat = "xml";
  // const outputFormat = "csv";

  anprFunc(tsanpr, path.join(imageDir, "licensePlate.jpg"), outputFormat, ""); // Single license plate recognition (default)
  anprFunc(tsanpr, path.join(imageDir, "multiple.jpg"), outputFormat, "vm"); // Recognize multiple license plates attached to vehicles
  anprFunc(tsanpr, path.join(imageDir, "multiple.jpg"), outputFormat, "vmb"); // Recognize multiple license plates including motorcycles
  anprFunc(tsanpr, path.join(imageDir, "surround.jpg"), outputFormat, "vms"); // Recognize multiple license plates with surround detection
  anprFunc(tsanpr, path.join(imageDir, "surround.jpg"), outputFormat, "dms"); // Recognize multiple surrounding objects (vehicles)
  anprFunc(tsanpr, path.join(imageDir, "surround.jpg"), outputFormat, "dmsr"); // Recognize multiple surrounding objects and license plates

  // Recognize multiple surrounding objects and license plates within RoI
  anprFunc(tsanpr, path.join(imageDir, "surround.jpg"), outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700");

  return 0;
}

// Main entry point
function main(): void {
  const engineFileName = getEngineFileName();
  const tsanpr = loadTSANPR(engineFileName);

  // Try each country code as needed
  readLicensePlates(tsanpr, "KR");
  // readLicensePlates(tsanpr, "JP");
  // readLicensePlates(tsanpr, "VN");
}

main();
