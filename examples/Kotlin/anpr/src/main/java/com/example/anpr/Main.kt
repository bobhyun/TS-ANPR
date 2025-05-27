package com.example.anpr

import java.nio.file.Files
import java.nio.file.Paths
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.io.File
import java.io.IOException
import java.io.PrintStream
import java.nio.charset.StandardCharsets
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.image.DataBufferByte
import java.awt.image.DataBufferInt

/**
 * Functional interface for ANPR function selection.
 */
fun interface AnprFunction {
    fun apply(tsanpr: TSANPR, imgfile: String, outputFormat: String, options: String)
}

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
 * ANPR Kotlin Example
 * This example demonstrates how to use the TSANPR engine via JNI.
 */
object Main {
    // Base directory for examples
    private const val examplesBaseDir = "../.."

    /**
     * Generate JNI wrapper filename depending on platform.
     */
    fun getJniFileName(): String {
        val os = System.getProperty("os.name").lowercase()
        val arch = System.getProperty("os.arch").lowercase()
        val currentWorkingDir = "." // Project root directory

        return when {
            os.contains("win") -> {
                if (arch.contains("64")) {
                    // Windows 64bit
                    "$currentWorkingDir/bin/windows-x86_64/jni/Debug/tsanpr_jni.dll"
                    // return "$currentWorkingDir/bin/windows-x86_64/jni/Release/tsanpr_jni.dll"
                } else {
                    // Windows 32bit
                    "$currentWorkingDir/bin/windows-x86/jni/Debug/tsanpr_jni.dll"
                    // return "$currentWorkingDir/bin/windows-x86/jni/Release/tsanpr_jni.dll"
                }
            }
            os.contains("linux") -> {
                when {
                    arch.contains("aarch64") -> {
                        // Linux ARM64
                        "$currentWorkingDir/bin/linux-aarch64/jni/libtsanpr_jni.so"
                    }
                    arch.contains("64") -> {
                        // Linux x86_64
                        "$currentWorkingDir/bin/linux-x86_64/jni/libtsanpr_jni.so"
                    }
                    else -> throw RuntimeException("Unsupported OS/arch: $os/$arch")
                }
            }
            else -> throw RuntimeException("Unsupported OS/arch: $os/$arch")
        }
    }

    /**
     * Generate engine filename depending on platform.
     */
    fun getEngineFileName(): String {
        val os = System.getProperty("os.name").lowercase()
        val arch = System.getProperty("os.arch").lowercase()

        return when {
            os.contains("win") -> {
                if (arch.contains("64")) {
                    // Windows 64bit
                    "$examplesBaseDir/bin/windows-x86_64/tsanpr.dll"
                } else {
                    // Windows 32bit
                    "$examplesBaseDir/bin/windows-x86/tsanpr.dll"
                }
            }
            os.contains("linux") -> {
                when {
                    arch.contains("aarch64") -> {
                        // Linux ARM64
                        "$examplesBaseDir/bin/linux-aarch64/jni/libtsanpr_jni.so"
                    }
                    arch.contains("64") -> {
                        // Linux x86_64
                        "$examplesBaseDir/bin/linux-x86_64/jni/libtsanpr_jni.so"
                    }
                    else -> throw RuntimeException("Unsupported OS/arch: $os/$arch")
                }
            }
            else -> throw RuntimeException("Unsupported OS/arch: $os/$arch")
        }
    }

    /**
     * Recognize license plate from image file.
     */
    fun readImageFile(tsanpr: TSANPR, imgfile: String, outputFormat: String, options: String) {
        System.out.printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options)
        val result = tsanpr.anpr_read_file(imgfile, outputFormat, options)
        println(result)
    }

    /**
     * Recognize license plate from encoded image buffer.
     */
    fun readEncodedImage(tsanpr: TSANPR, imgfile: String, outputFormat: String, options: String) {
        System.out.printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options)
        val encodedImg: ByteArray = try {
            Files.readAllBytes(Paths.get(imgfile))
        } catch (e: IOException) {
            println("File open failed")
            return
        }
        val result = tsanpr.anpr_read_pixels(
            encodedImg,
            encodedImg.size.toLong(),
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
    fun readPixelBuffer(tsanpr: TSANPR, imgfile: String, outputFormat: String, options: String) {
        System.out.printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options)
        val img: BufferedImage = try {
            ImageIO.read(File(imgfile)) ?: run {
                println("Image load failed! (Unsupported format or file not found)")
                return
            }
        } catch (e: IOException) {
            println("Image load failed! (${e.message})")
            return
        }

        val width = img.width
        val height = img.height
        val stride: Int
        val pixelFormat: String
        val pixels: ByteArray

        when (img.type) {
            BufferedImage.TYPE_3BYTE_BGR -> {
                pixelFormat = "BGR"
                stride = width * 3
                pixels = (img.raster.dataBuffer as DataBufferByte).data
            }
            BufferedImage.TYPE_4BYTE_ABGR -> {
                pixelFormat = "BGRA"
                stride = width * 4
                pixels = (img.raster.dataBuffer as DataBufferByte).data
            }
            BufferedImage.TYPE_BYTE_GRAY -> {
                pixelFormat = "GRAY"
                stride = width
                pixels = (img.raster.dataBuffer as DataBufferByte).data
            }
            BufferedImage.TYPE_INT_RGB -> {
                // 0x00RRGGBB, stored in memory as BGRA (alpha = 0)
                val intPixels = (img.raster.dataBuffer as DataBufferInt).data
                val byteBuffer = ByteBuffer.allocate(intPixels.size * 4)
                byteBuffer.order(ByteOrder.nativeOrder())
                byteBuffer.asIntBuffer().put(intPixels)
                pixels = byteBuffer.array()
                pixelFormat = "BGRA"
                stride = width * 4
            }
            BufferedImage.TYPE_INT_BGR -> {
                // 0x00BBGGRR, stored in memory as RGBA (alpha = 0)
                val intPixels = (img.raster.dataBuffer as DataBufferInt).data
                val byteBuffer = ByteBuffer.allocate(intPixels.size * 4)
                byteBuffer.order(ByteOrder.nativeOrder())
                byteBuffer.asIntBuffer().put(intPixels)
                pixels = byteBuffer.array()
                pixelFormat = "RGBA"
                stride = width * 4
            }
            else -> {
                println("Unsupported BufferedImage type: ${img.type}")
                return
            }
        }

        val result = tsanpr.anpr_read_pixels(
            pixels,
            width.toLong(),
            height.toLong(),
            stride.toLong(),
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
    fun readLicensePlates(tsanpr: TSANPR, countryCode: String): Int {
        val initParams = "text;country=$countryCode"
        val error = tsanpr.anpr_initialize(initParams)
        if (!error.isNullOrEmpty()) {
            System.out.printf("anpr_initialize() failed (error=%s)\n", error)
            return -1
        }

        val imageDir = "$examplesBaseDir/img/$countryCode/"

        // TODO: Try each function as needed
        val anprFunc: AnprFunction = AnprFunction { t, f, o, op -> readImageFile(t, f, o, op) }
        // val anprFunc: AnprFunction = AnprFunction { t, f, o, op -> readEncodedImage(t, f, o, op) }
        // val anprFunc: AnprFunction = AnprFunction { t, f, o, op -> readPixelBuffer(t, f, o, op) }

        // TODO: Try each output format as needed
        var outputFormat = "text"
        // outputFormat = "json"
        // outputFormat = "yaml"
        // outputFormat = "xml"
        // outputFormat = "csv"

        anprFunc.apply(tsanpr, imageDir + "licensePlate.jpg", outputFormat, "") // Single license plate recognition (default)
        anprFunc.apply(tsanpr, imageDir + "multiple.jpg", outputFormat, "vm") // Recognize multiple license plates attached to vehicles
        anprFunc.apply(tsanpr, imageDir + "multiple.jpg", outputFormat, "vmb") // Recognize multiple license plates including motorcycles
        anprFunc.apply(tsanpr, imageDir + "surround.jpg", outputFormat, "vms") // Recognize multiple license plates with surround detection
        anprFunc.apply(tsanpr, imageDir + "surround.jpg", outputFormat, "dms") // Recognize multiple surrounding objects (vehicles)
        anprFunc.apply(tsanpr, imageDir + "surround.jpg", outputFormat, "dmsr") // Recognize multiple surrounding objects and license plates

        return 0
    }

    @JvmStatic
    fun main(args: Array<String>) {
        // Set UTF-8 encoding for console output
        System.setOut(PrintStream(System.out, true, StandardCharsets.UTF_8))
        System.setErr(PrintStream(System.err, true, StandardCharsets.UTF_8))
        println("Working directory: " + System.getProperty("user.dir"))

        val jniPath = getJniFileName()
        val enginePath = getEngineFileName()
        val absJniPath = File(jniPath).absolutePath
        println("JNI wrapper absolute path: $absJniPath")
        val absEnginePath = File(enginePath).absolutePath
        println("Engine absolute path: $absEnginePath")

        // Use try-with-resources to ensure native resources are released safely
        TSANPR(absJniPath, absEnginePath).use { tsanpr ->
            // TODO: Try each country code as needed
            readLicensePlates(tsanpr, "KR")
            // readLicensePlates(tsanpr, "JP")
            // readLicensePlates(tsanpr, "VN")
        }
    }
}
