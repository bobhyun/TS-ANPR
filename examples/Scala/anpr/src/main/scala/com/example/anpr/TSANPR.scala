package com.example.anpr

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
 */

/**
 * TSANPR Java JNI Wrapper
 *
 * This class provides native method declarations for the TSANPR engine.
 * It also allows dynamic loading of the JNI wrapper and passing the engine DLL/so path from Java.
 */
class TSANPR(jniWrapperPath: String, enginePath: String) extends AutoCloseable {
  private var loaded = false

  /**
   * Loads the JNI wrapper and initializes the engine with the given engine library path.
   * @param jniWrapperPath Full path to the JNI wrapper DLL/SO file
   * @param enginePath Full path to the engine DLL/SO file
   */
  if (jniWrapperPath == null || jniWrapperPath.isEmpty)
    throw new RuntimeException("jniWrapperPath is empty!")

  if (enginePath == null || enginePath.isEmpty)
    throw new RuntimeException("enginePath is empty!")

  System.load(jniWrapperPath)
  val ret = nativeInit(enginePath)
  if (ret < 0) throw new RuntimeException(s"Engine load failed (code=$ret)")
  loaded = true

  /**
   * Calls TSANPR_unload in native code.
   */
  override def close(): Unit = {
    if (loaded) {
      nativeRelease()
      loaded = false
    }
  }

  // Native methods
  @native private def nativeInit(enginePath: String): Int
  @native private def nativeRelease(): Unit

  /**
   * Initializes the TSANPR engine with the specified mode.
   * @param mode Operation mode string (e.g. "text;country=KR")
   * @return Error message if failed, or null/empty string on success
   */
  @native def anpr_initialize(mode: String): String

  /**
   * Recognizes license plates from an image file.
   * @param imgFileName Path to the image file
   * @param outputFormat Output data format (e.g. "text", "json")
   * @param options Feature options string
   * @return Recognition result string
   */
  @native def anpr_read_file(imgFileName: String, outputFormat: String, options: String): String

  /**
   * Recognizes license plates from an image pixel buffer.
   * @param pixels Image pixel buffer (encoded or raw)
   * @param width Image width (pixels or buffer size for encoded)
   * @param height Image height (pixels, 0 for encoded)
   * @param stride Bytes per image line (0 for encoded)
   * @param pixelFormat Pixel format string ("encoded", "BGR", "GRAY", etc.)
   * @param outputFormat Output data format
   * @param options Feature options string
   * @return Recognition result string
   */
  @native def anpr_read_pixels(
    pixels: Array[Byte],
    width: Long,
    height: Long,
    stride: Long,
    pixelFormat: String,
    outputFormat: String,
    options: String
  ): String
}
