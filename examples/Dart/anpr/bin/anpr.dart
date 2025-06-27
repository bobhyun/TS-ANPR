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
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

import 'dart:io';
import 'dart:ffi';
import 'dart:typed_data';
import 'package:ffi/ffi.dart';
import 'package:image/image.dart' as img;
import 'package:anpr/tsanpr.dart';


// Base directory for examples
const String examplesBaseDir = '../..';

// Generate engine filename depending on platform
String getEngineFileName() {
  if (Platform.isWindows) {
    return '$examplesBaseDir/bin/windows-x86_64/tsanpr.dll';
  } else if (Platform.isLinux) {
    return '$examplesBaseDir/bin/linux-x86_64/libtsanpr.so';
  } else if (Platform.isMacOS) {
    return '$examplesBaseDir/bin/macos-x86_64/libtsanpr.dylib';
  }
  throw UnsupportedError('Unsupported platform');
}

/// Recognize license plate from image file
void readImageFile(TSANPR tsanpr, String imgfile, String outputFormat, String options) {
  print('$imgfile (outputFormat="$outputFormat", options="$options") => ');
  final resultPtr = tsanpr.anpr_read_file(
    imgfile.toNativeUtf8(),
    outputFormat.toNativeUtf8(),
    options.toNativeUtf8(),
  );
  print(resultPtr.toDartString());
}

/// Recognize license plate from encoded image (e.g., JPEG/PNG)
void readEncodedImage(TSANPR tsanpr, String imgfile, String outputFormat, String options) {
  print('$imgfile (outputFormat="$outputFormat", options="$options") => ');
  final fileBytes = File(imgfile).readAsBytesSync();
  final ptr = calloc<Uint8>(fileBytes.length);
  final buf = ptr.asTypedList(fileBytes.length);
  buf.setAll(0, fileBytes);

  final resultPtr = tsanpr.anpr_read_pixels(
    ptr,
    fileBytes.length,
    0,
    0,
    'encoded'.toNativeUtf8(),
    outputFormat.toNativeUtf8(),
    options.toNativeUtf8(),
  );
  print(resultPtr.toDartString());
  calloc.free(ptr);
}

/// Recognize license plate from pixel buffer
void readPixelBuffer(
  TSANPR tsanpr,
  String imgfile,
  String outputFormat,
  String options,
) {
  print('$imgfile (outputFormat="$outputFormat", options="$options") => ');

  final bytes = File(imgfile).readAsBytesSync();
  final image = img.decodeImage(bytes);

  if (image == null) {
    print('Image load failed!');
    return;
  }

  final Uint8List pixelBytes = image.getBytes(order: img.ChannelOrder.bgra);

  final pixelPtr = calloc<Uint8>(pixelBytes.length);
  pixelPtr.asTypedList(pixelBytes.length).setAll(0, pixelBytes);

  final stride = image.width * 4;
  final pixelFormat = 'BGRA';


  final resultPtr = tsanpr.anpr_read_pixels(
    pixelPtr,
    image.width,
    image.height,
    stride,
    pixelFormat.toNativeUtf8(),
    outputFormat.toNativeUtf8(),
    options.toNativeUtf8(),
  );
  print(resultPtr.toDartString());

  calloc.free(pixelPtr);
}

/// Recognize license plates for a given country code
int readLicensePlates(TSANPR tsanpr, String countryCode) {
  // NOTICE:
  // anpr_initialize should be called only once after library load.
  // Therefore, it is not possible to change the country code after anpr_initialize has been called.
  // While using the free trial license, you can try all languages.
  // When you purchase a commercial license, you can only use the selected language.

  final initParams = 'text;country=$countryCode';
  final errorPtr = tsanpr.anpr_initialize(initParams.toNativeUtf8());
  final error = errorPtr.toDartString();
  if (error.isNotEmpty) {
    print('anpr_initialize() failed (error=$error)');
    return -1;
  }

  final imageDir = '$examplesBaseDir/img/$countryCode';
  // TODO: Try each function as needed
  final anprFunc = readImageFile;
  //final anprFunc = readEncodedImage;
  //final anprFunc = readPixelBuffer;

  // TODO: Try each output format as needed
  final outputFormat = 'text';
  // const outputFormat = 'json';
  // const outputFormat = 'yaml';
  // const outputFormat = 'xml';
  // const outputFormat = 'csv';

  anprFunc(tsanpr, '$imageDir/licensePlate.jpg', outputFormat, ''); // Single license plate recognition (default)
  anprFunc(tsanpr, '$imageDir/multiple.jpg', outputFormat, 'vm'); // Recognize multiple license plates attached to vehicles
  anprFunc(tsanpr, '$imageDir/multiple.jpg', outputFormat, 'vmb'); // Recognize multiple license plates including motorcycles
  anprFunc(tsanpr, '$imageDir/surround.jpg', outputFormat, 'vms'); // Recognize multiple license plates with surround detection
  anprFunc(tsanpr, '$imageDir/surround.jpg', outputFormat, 'dms'); // Recognize multiple surrounding objects (vehicles)
  anprFunc(tsanpr, '$imageDir/surround.jpg', outputFormat, 'dmsr'); // Recognize multiple surrounding objects and license plates
  // Recognize multiple surrounding objects and license plates within RoI
  anprFunc(tsanpr, '$imageDir/surround.jpg', outputFormat, 'dmsri549,700,549,2427,1289,2427,1289,700');

  return 0;
}

void main(List<String> arguments) {
  final engineFileName = getEngineFileName();
  final tsanpr = TSANPR(engineFileName);

  // TODO: Try each country code as needed
  readLicensePlates(tsanpr, 'KR');
  // readLicensePlates(tsanpr, 'JP');
  // readLicensePlates(tsanpr, 'VN');
}
