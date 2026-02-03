#!/usr/bin/env groovy
/*
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

@Grab('net.java.dev.jna:jna:5.14.0')
import com.sun.jna.Memory
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.image.DataBufferByte
import java.awt.image.DataBufferInt
import java.nio.ByteBuffer
import java.nio.ByteOrder
import groovy.transform.Field

// Load TSANPR module
def scriptDir = new File(getClass().protectionDomain.codeSource.location.toURI()).parent
def loader = new GroovyClassLoader(getClass().classLoader)
def tsanprFile = new File(scriptDir, 'TSANPR.groovy')
loader.parseClass(tsanprFile)
def tsanprClass = loader.loadClass('TSANPR')

@Field static final String EXAMPLES_BASE_DIR = "../.."

/**
 * Get engine file path based on OS and architecture.
 */
def getEngineFileName() {
    def os = System.getProperty("os.name").toLowerCase()
    def arch = System.getProperty("os.arch").toLowerCase()

    if (os.contains("win")) {
        if (arch.contains("64")) {
            return "${EXAMPLES_BASE_DIR}/bin/windows-x86_64/tsanpr.dll"
        } else {
            return "${EXAMPLES_BASE_DIR}/bin/windows-x86/tsanpr.dll"
        }
    } else if (os.contains("linux")) {
        if (arch.contains("aarch64")) {
            return "${EXAMPLES_BASE_DIR}/bin/linux-aarch64/libtsanpr.so"
        } else if (arch.contains("64")) {
            return "${EXAMPLES_BASE_DIR}/bin/linux-x86_64/libtsanpr.so"
        }
    }
    throw new RuntimeException("Unsupported OS/arch: ${os}/${arch}")
}

/**
 * Recognize license plate from image file.
 */
def readImageFile(tsanpr, String imgFile, String outputFormat, String options) {
    print "${imgFile} (outputFormat=\"${outputFormat}\", options=\"${options}\") => "
    def result = tsanpr.readFile(imgFile, outputFormat, options)
    println result
}

/**
 * Recognize license plate from encoded image buffer.
 */
def readEncodedImage(tsanpr, String imgFile, String outputFormat, String options) {
    print "${imgFile} (outputFormat=\"${outputFormat}\", options=\"${options}\") => "

    def file = new File(imgFile)
    if (!file.exists()) {
        println "File not found"
        return
    }

    byte[] encodedImg = file.bytes
    Memory memory = new Memory(encodedImg.length)
    memory.write(0, encodedImg, 0, encodedImg.length)

    def result = tsanpr.readPixels(
        memory,
        encodedImg.length as long,
        0L,
        0L,
        "encoded",
        outputFormat,
        options
    )
    println result
}

/**
 * Recognize license plate from pixel buffer.
 */
def readPixelBuffer(tsanpr, String imgFile, String outputFormat, String options) {
    print "${imgFile} (outputFormat=\"${outputFormat}\", options=\"${options}\") => "

    BufferedImage img
    try {
        img = ImageIO.read(new File(imgFile))
        if (img == null) {
            println "Image load failed! (Unsupported format or file not found)"
            return
        }
    } catch (IOException e) {
        println "Image load failed! (${e.message})"
        return
    }

    int width = img.width
    int height = img.height
    int stride
    String pixelFormat
    byte[] pixels

    switch (img.type) {
        case BufferedImage.TYPE_3BYTE_BGR:
            pixelFormat = "BGR"
            stride = width * 3
            pixels = ((DataBufferByte) img.raster.dataBuffer).data
            break
        case BufferedImage.TYPE_4BYTE_ABGR:
            pixelFormat = "BGRA"
            stride = width * 4
            pixels = ((DataBufferByte) img.raster.dataBuffer).data
            break
        case BufferedImage.TYPE_BYTE_GRAY:
            pixelFormat = "GRAY"
            stride = width
            pixels = ((DataBufferByte) img.raster.dataBuffer).data
            break
        case BufferedImage.TYPE_INT_RGB:
            // 0x00RRGGBB, stored in memory as BGRA (alpha = 0)
            int[] intPixels = ((DataBufferInt) img.raster.dataBuffer).data
            ByteBuffer byteBuffer = ByteBuffer.allocate(intPixels.length * 4)
            byteBuffer.order(ByteOrder.nativeOrder())
            byteBuffer.asIntBuffer().put(intPixels)
            pixels = byteBuffer.array()
            pixelFormat = "BGRA"
            stride = width * 4
            break
        case BufferedImage.TYPE_INT_BGR:
            // 0x00BBGGRR, stored in memory as RGBA (alpha = 0)
            int[] intPixelsBgr = ((DataBufferInt) img.raster.dataBuffer).data
            ByteBuffer byteBufferBgr = ByteBuffer.allocate(intPixelsBgr.length * 4)
            byteBufferBgr.order(ByteOrder.nativeOrder())
            byteBufferBgr.asIntBuffer().put(intPixelsBgr)
            pixels = byteBufferBgr.array()
            pixelFormat = "RGBA"
            stride = width * 4
            break
        default:
            println "Unsupported BufferedImage type: ${img.type}"
            return
    }

    Memory memory = new Memory(pixels.length)
    memory.write(0, pixels, 0, pixels.length)

    def result = tsanpr.readPixels(
        memory,
        width as long,
        height as long,
        stride as long,
        pixelFormat,
        outputFormat,
        options
    )
    println result
}

/**
 * Recognize license plates for a specific country code.
 */
def readLicensePlates(tsanpr, String countryCode) {
    // NOTICE:
    // anpr_initialize should be called only once after library load.
    // Therefore, it is not possible to change the country code after anpr_initialize has been called.
    // While using the free trial license, you can try all languages.
    // When you purchase a commercial license, you can only use the selected language.
    def initParams = "text;country=${countryCode}"
    def error = tsanpr.initialize(initParams)
    if (error) {
        println "anpr_initialize() failed (error=${error})"
        return -1
    }

    def imageDir = "${EXAMPLES_BASE_DIR}/img/${countryCode}/"

    // TODO: Try each function as needed
    def anprFunc = this.&readImageFile
    // def anprFunc = this.&readEncodedImage
    // def anprFunc = this.&readPixelBuffer

    // TODO: Try each output format as needed
    def outputFormat = "text"
    // def outputFormat = "json"
    // def outputFormat = "yaml"
    // def outputFormat = "xml"
    // def outputFormat = "csv"

    anprFunc(tsanpr, "${imageDir}licensePlate.jpg", outputFormat, "")     // Single license plate recognition (default)
    anprFunc(tsanpr, "${imageDir}multiple.jpg", outputFormat, "vm")       // Recognize multiple license plates attached to vehicles
    anprFunc(tsanpr, "${imageDir}multiple.jpg", outputFormat, "vmb")      // Recognize multiple license plates including motorcycles
    anprFunc(tsanpr, "${imageDir}surround.jpg", outputFormat, "vms")      // Recognize multiple license plates with surround detection
    anprFunc(tsanpr, "${imageDir}surround.jpg", outputFormat, "dms")      // Recognize multiple surrounding objects (vehicles)
    anprFunc(tsanpr, "${imageDir}surround.jpg", outputFormat, "dmsr")     // Recognize multiple surrounding objects and license plates

    // Recognize multiple surrounding objects and license plates within RoI
    anprFunc(tsanpr, "${imageDir}surround.jpg", outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700")

    return 0
}

// ============================================================================
// Main
// ============================================================================

def enginePath = new File(getEngineFileName()).canonicalPath
println "Engine path: ${enginePath}"

if (!new File(enginePath).exists()) {
    println "Engine file not found: ${enginePath}"
    return
}

def tsanpr = tsanprClass.newInstance(enginePath)

// TODO: Try each country code as needed
readLicensePlates(tsanpr, "KR")
// readLicensePlates(tsanpr, "JP")
// readLicensePlates(tsanpr, "VN")
