package com.example.anpr

import java.nio.file.{Files, Paths}
import java.nio.{ByteBuffer, ByteOrder}
import java.io.{File, IOException, PrintStream}
import java.nio.charset.StandardCharsets
import javax.imageio.ImageIO
import java.awt.image.{BufferedImage, DataBufferByte, DataBufferInt}

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
 * ANPR Java Example
 * This example demonstrates how to use the TSANPR engine via JNI.
 */
object Main {
  // Base directory for examples
  private val examplesBaseDir = "../.."

  /**
   * Generate JNI wrapper filename depending on platform.
   */
  def getJniFileName(): String = {
    val os = System.getProperty("os.name").toLowerCase
    val arch = System.getProperty("os.arch").toLowerCase
    val currentWorkingDir = "." // Project root directory

    if (os.contains("win")) {
      if (arch.contains("64")) {
        // Windows 64bit
        s"$currentWorkingDir/bin/windows-x86_64/jni/Debug/tsanpr_jni.dll"
        // Or use Release version if needed
        // s"$currentWorkingDir/bin/windows-x86_64/jni/Release/tsanpr_jni.dll"
      } else {
        // Windows 32bit
        s"$currentWorkingDir/bin/windows-x86/jni/Debug/tsanpr_jni.dll"
        // Or use Release version if needed
        // s"$currentWorkingDir/bin/windows-x86/jni/Release/tsanpr_jni.dll"
      }
    } else if (os.contains("linux")) {
      if (arch.contains("aarch64")) {
        // Linux ARM64
        s"$currentWorkingDir/bin/linux-aarch64/jni/libtsanpr_jni.so"
      } else if (arch.contains("64")) {
        // Linux x86_64
        s"$currentWorkingDir/bin/linux-x86_64/jni/libtsanpr_jni.so"
      } else {
        throw new RuntimeException(s"Unsupported OS/arch: $os/$arch")
      }
    } else {
      throw new RuntimeException(s"Unsupported OS/arch: $os/$arch")
    }
  }

  /**
   * Generate engine filename depending on platform.
   */
  def getEngineFileName(): String = {
    val os = System.getProperty("os.name").toLowerCase
    val arch = System.getProperty("os.arch").toLowerCase

    if (os.contains("win")) {
      if (arch.contains("64")) {
        // Windows 64bit
        s"$examplesBaseDir/bin/windows-x86_64/tsanpr.dll"
      } else {
        // Windows 32bit
        s"$examplesBaseDir/bin/windows-x86/tsanpr.dll"
      }
    } else if (os.contains("linux")) {
      if (arch.contains("aarch64")) {
        // Linux ARM64
        s"$examplesBaseDir/bin/linux-aarch64/jni/libtsanpr_jni.so"
      } else if (arch.contains("64")) {
        // Linux x86_64
        s"$examplesBaseDir/bin/linux-x86_64/jni/libtsanpr_jni.so"
      } else {
        throw new RuntimeException(s"Unsupported OS/arch: $os/$arch")
      }
    } else {
      throw new RuntimeException(s"Unsupported OS/arch: $os/$arch")
    }
  }

  /**
   * Recognize license plate from image file.
   */
  def readImageFile(tsanpr: TSANPR, imgfile: String, outputFormat: String, options: String): Unit = {
    printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options)
    val result = tsanpr.anpr_read_file(imgfile, outputFormat, options)
    println(result)
  }

  /**
   * Recognize license plate from encoded image buffer.
   */
  def readEncodedImage(tsanpr: TSANPR, imgfile: String, outputFormat: String, options: String): Unit = {
    printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options)
    val encodedImg =
      try {
        Files.readAllBytes(Paths.get(imgfile))
      } catch {
        case _: IOException =>
          println("File open failed")
          return
      }
    val result = tsanpr.anpr_read_pixels(
      encodedImg,
      encodedImg.length,
      0,
      0,
      "encoded",
      outputFormat,
      options
    )
    println(result)
  }

  /**
   * Recognize license plate from pixel buffer.
   */
  def readPixelBuffer(tsanpr: TSANPR, imgfile: String, outputFormat: String, options: String): Unit = {
    printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options)
    val img =
      try {
        val image = ImageIO.read(new File(imgfile))
        if (image == null) {
          println("Image load failed! (Unsupported format or file not found)")
          return
        }
        image
      } catch {
        case e: IOException =>
          println(s"Image load failed! (${e.getMessage})")
          return
      }

    val width = img.getWidth
    val height = img.getHeight
    var stride = 0
    var pixelFormat = ""
    var pixels: Array[Byte] = null

    img.getType match {
      case BufferedImage.TYPE_3BYTE_BGR =>
        pixelFormat = "BGR"
        stride = width * 3
        pixels = img.getRaster.getDataBuffer.asInstanceOf[DataBufferByte].getData
      case BufferedImage.TYPE_4BYTE_ABGR =>
        pixelFormat = "BGRA"
        stride = width * 4
        pixels = img.getRaster.getDataBuffer.asInstanceOf[DataBufferByte].getData
      case BufferedImage.TYPE_BYTE_GRAY =>
        pixelFormat = "GRAY"
        stride = width
        pixels = img.getRaster.getDataBuffer.asInstanceOf[DataBufferByte].getData
      case BufferedImage.TYPE_INT_RGB =>
        // 0x00RRGGBB, stored in memory as BGRA (alpha = 0)
        val intPixels = img.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
        val byteBuffer = ByteBuffer.allocate(intPixels.length * 4)
        byteBuffer.order(ByteOrder.nativeOrder())
        byteBuffer.asIntBuffer().put(intPixels)
        pixels = byteBuffer.array()
        pixelFormat = "BGRA"
        stride = width * 4
      case BufferedImage.TYPE_INT_BGR =>
        // 0x00BBGGRR, stored in memory as RGBA (alpha = 0)
        val intPixels = img.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
        val byteBuffer = ByteBuffer.allocate(intPixels.length * 4)
        byteBuffer.order(ByteOrder.nativeOrder())
        byteBuffer.asIntBuffer().put(intPixels)
        pixels = byteBuffer.array()
        pixelFormat = "RGBA"
        stride = width * 4
      case other =>
        println(s"Unsupported BufferedImage type: $other")
        return
    }

    val result = tsanpr.anpr_read_pixels(
      pixels,
      width,
      height,
      stride,
      pixelFormat,
      outputFormat,
      options
    )
    println(result)
  }

  /**
   * Recognize license plates for a specific country code.
   *
   * NOTICE:
   * anpr_initialize should be called only once after library load.
   * Therefore, it is not possible to change the country code after anpr_initialize has been called.
   * While using the free trial license, you can try all languages.
   * When you purchase a commercial license, you can only use the selected language.
   */
  def readLicensePlates(tsanpr: TSANPR, countryCode: String): Int = {
    val initParams = s"text;country=$countryCode"
    val error = tsanpr.anpr_initialize(initParams)
    if (error != null && error.nonEmpty) {
      printf("anpr_initialize() failed (error=%s)\n", error)
      return -1
    }

    val imageDir = s"$examplesBaseDir/img/$countryCode/"

    // TODO: Try each function as needed
    val anprFunc: (TSANPR, String, String, String) => Unit = readImageFile
    // val anprFunc: (TSANPR, String, String, String) => Unit = readEncodedImage
    // val anprFunc: (TSANPR, String, String, String) => Unit = readPixelBuffer

    // TODO: Try each output format as needed
    val outputFormat = "text"
    // val outputFormat = "json"
    // val outputFormat = "yaml"
    // val outputFormat = "xml"
    // val outputFormat = "csv"

    anprFunc(tsanpr, imageDir + "licensePlate.jpg", outputFormat, "") // Single license plate recognition (default)
    anprFunc(tsanpr, imageDir + "multiple.jpg", outputFormat, "vm")   // Recognize multiple license plates attached to vehicles
    anprFunc(tsanpr, imageDir + "multiple.jpg", outputFormat, "vmb")  // Recognize multiple license plates including motorcycles
    anprFunc(tsanpr, imageDir + "surround.jpg", outputFormat, "vms")  // Recognize multiple license plates with surround detection
    anprFunc(tsanpr, imageDir + "surround.jpg", outputFormat, "dms")  // Recognize multiple surrounding objects (vehicles)
    anprFunc(tsanpr, imageDir + "surround.jpg", outputFormat, "dmsr") // Recognize multiple surrounding objects and license plates
    // Recognize multiple surrounding objects and license plates within RoI
    anprFunc(tsanpr, imageDir + "surround.jpg", outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700")

    0
  }

  /**
   * Functional interface for ANPR function selection.
   */
  type AnprFunction = (TSANPR, String, String, String) => Unit

  def main(args: Array[String]): Unit = {
    // Set UTF-8 encoding for console output
    System.setOut(new PrintStream(System.out, true, StandardCharsets.UTF_8))
    System.setErr(new PrintStream(System.err, true, StandardCharsets.UTF_8))
    println("Working directory: " + System.getProperty("user.dir"))

    val jniPath = getJniFileName()
    val enginePath = getEngineFileName()

    // Convert to absolute path and print
    val absJniPath = new File(jniPath).getAbsolutePath
    println("JNI wrapper absolute path: " + absJniPath)
    val absEnginePath = new File(enginePath).getAbsolutePath
    println("Engine absolute path: " + absEnginePath)

    // Use try-with-resources to ensure native resources are released safely
    val tsanpr = new TSANPR(absJniPath, absEnginePath)
    try {
      // TODO: Try each country code as needed
      readLicensePlates(tsanpr, "KR")
      // readLicensePlates(tsanpr, "JP")
      // readLicensePlates(tsanpr, "VN")
    } finally {
      tsanpr.close()
      // tsanpr.close() is automatically called here
    }
  }
}
