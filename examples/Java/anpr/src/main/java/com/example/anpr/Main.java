package com.example.anpr;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.ByteBuffer;
import java.nio.IntBuffer;
import java.nio.ByteOrder;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferInt;


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
public class Main {

    // Base directory for examples
    private static final String examplesBaseDir = "../..";

    /**
     * Generate JNI wrapper filename depending on platform.
     */
    public static String getJniFileName() {
        String os = System.getProperty("os.name").toLowerCase();
        String arch = System.getProperty("os.arch").toLowerCase();

        String currentWorkingDir = "."; // Project root directory
        if (os.contains("win")) {
            if (arch.contains("64")) {
                // Windows 64bit
                return currentWorkingDir + "/bin/windows-x86_64/jni/Debug/tsanpr_jni.dll";
                //return currentWorkingDir + "/bin/windows-x86_64/jni/Release/tsanpr_jni.dll";
            } else {
                // Windows 32bit
                return currentWorkingDir + "/bin/windows-x86/jni/Debug/tsanpr_jni.dll";
                //return currentWorkingDir + "/bin/windows-x86/jni/Release/tsanpr_jni.dll";
            }
        } else if (os.contains("linux")) {
            if (arch.contains("aarch64")) {
                // Linux ARM64
                return currentWorkingDir + "/bin/linux-aarch64/jni/libtsanpr_jni.so";
            } else if (arch.contains("64")) {
                // Linux x86_64
                return currentWorkingDir + "/bin/linux-x86_64/jni/libtsanpr_jni.so";
            }
        }
        throw new RuntimeException("Unsupported OS/arch: " + os + "/" + arch);
    }

    /**
     * Generate engine filename depending on platform.
     */
    public static String getEngineFileName() {
        String os = System.getProperty("os.name").toLowerCase();
        String arch = System.getProperty("os.arch").toLowerCase();

        if (os.contains("win")) {
            if (arch.contains("64")) {
                // Windows 64bit
                return examplesBaseDir + "/bin/windows-x86_64/tsanpr.dll";
            } else {
                // Windows 32bit
                return examplesBaseDir + "/bin/windows-x86/tsanpr.dll";
            }
        } else if (os.contains("linux")) {
            if (arch.contains("aarch64")) {
                // Linux ARM64
                return examplesBaseDir + "/bin/linux-aarch64/jni/libtsanpr_jni.so";
            } else if (arch.contains("64")) {
                // Linux x86_64
                return examplesBaseDir + "/bin/linux-x86_64/jni/libtsanpr_jni.so";
            }
        }
        throw new RuntimeException("Unsupported OS/arch: " + os + "/" + arch);
    }

    /**
     * Recognize license plate from image file.
     */
    public static void readImageFile(TSANPR tsanpr, String imgfile, String outputFormat, String options) {
        System.out.printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options);
        String result = tsanpr.anpr_read_file(imgfile, outputFormat, options);
        System.out.println(result);
    }

    /**
     * Recognize license plate from encoded image buffer.
     */
    public static void readEncodedImage(TSANPR tsanpr, String imgfile, String outputFormat, String options) {
        System.out.printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options);
        byte[] encodedImg;
        try {
            encodedImg = Files.readAllBytes(Paths.get(imgfile));
        } catch (IOException e) {
            System.out.println("File open failed");
            return;
        }
        String result = tsanpr.anpr_read_pixels(
                encodedImg,
                encodedImg.length,
                0,
                0,
                "encoded",
                outputFormat,
                options
        );
        System.out.println(result);
    }

    /**
     * Recognize license plate from pixel buffer.
     */
    public static void readPixelBuffer(TSANPR tsanpr, String imgfile, String outputFormat, String options) {
        System.out.printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options);

        BufferedImage img;
        try {
            img = ImageIO.read(new File(imgfile));
            if (img == null) {
                System.out.println("Image load failed! (Unsupported format or file not found)");
                return;
            }
        } catch (IOException e) {
            System.out.println("Image load failed! (" + e.getMessage() + ")");
            return;
        }

        int width = img.getWidth();
        int height = img.getHeight();
        int stride;
        String pixelFormat;
        byte[] pixels;

        switch (img.getType()) {
            case BufferedImage.TYPE_3BYTE_BGR:
                pixelFormat = "BGR";
                stride = width * 3;
                pixels = ((DataBufferByte) img.getRaster().getDataBuffer()).getData();
                break;
            case BufferedImage.TYPE_4BYTE_ABGR:
                pixelFormat = "BGRA";
                stride = width * 4;
                pixels = ((DataBufferByte) img.getRaster().getDataBuffer()).getData();
                break;
            case BufferedImage.TYPE_BYTE_GRAY:
                pixelFormat = "GRAY";
                stride = width;
                pixels = ((DataBufferByte) img.getRaster().getDataBuffer()).getData();
                break;
            case BufferedImage.TYPE_INT_RGB: {
                // 0x00RRGGBB, stored in memory as BGRA (alpha = 0)
                int[] intPixels = ((DataBufferInt) img.getRaster().getDataBuffer()).getData();
                ByteBuffer byteBuffer = ByteBuffer.allocate(intPixels.length * 4);
                byteBuffer.order(ByteOrder.nativeOrder());
                byteBuffer.asIntBuffer().put(intPixels);
                pixels = byteBuffer.array();
                pixelFormat = "BGRA";
                stride = width * 4;
                break;
            }
            case BufferedImage.TYPE_INT_BGR: {
                // 0x00BBGGRR, stored in memory as RGBA (alpha = 0)
                int[] intPixels = ((DataBufferInt) img.getRaster().getDataBuffer()).getData();
                ByteBuffer byteBuffer = ByteBuffer.allocate(intPixels.length * 4);
                byteBuffer.order(ByteOrder.nativeOrder());
                byteBuffer.asIntBuffer().put(intPixels);
                pixels = byteBuffer.array();
                pixelFormat = "RGBA";
                stride = width * 4;
                break;
            }
            default:
                System.out.println("Unsupported BufferedImage type: " + img.getType());
                return;
        }

        String result = tsanpr.anpr_read_pixels(
                pixels,
                width,
                height,
                stride,
                pixelFormat,
                outputFormat,
                options
        );
        System.out.println(result);
    }

    /**
     * Recognize license plates for a specific country code.
     */
    public static int readLicensePlates(TSANPR tsanpr, String countryCode) {
        // NOTICE:
        // anpr_initialize should be called only once after library load.
        // Therefore, it is not possible to change the country code after anpr_initialize has been called.
        // While using the free trial license, you can try all languages.
        // When you purchase a commercial license, you can only use the selected language.
        String initParams = "text;country=" + countryCode;
        String error = tsanpr.anpr_initialize(initParams);
        if (error != null && !error.isEmpty()) {
            System.out.printf("anpr_initialize() failed (error=%s)\n", error);
            return -1;
        }

        String imageDir = examplesBaseDir + "/img/" + countryCode + "/";

        // TODO: Try each function as needed
        AnprFunction anprFunc = Main::readImageFile;
        // AnprFunction anprFunc = Main::readEncodedImage;
        // AnprFunction anprFunc = Main::readPixelBuffer;

        // TODO: Try each output format as needed
        String outputFormat = "text";
        // String outputFormat = "json";
        // String outputFormat = "yaml";
        // String outputFormat = "xml";
        // String outputFormat = "csv";

        anprFunc.apply(tsanpr, imageDir + "licensePlate.jpg", outputFormat, ""); // Single license plate recognition (default)
        anprFunc.apply(tsanpr, imageDir + "multiple.jpg", outputFormat, "vm"); // Recognize multiple license plates attached to vehicles
        anprFunc.apply(tsanpr, imageDir + "multiple.jpg", outputFormat, "vmb"); // Recognize multiple license plates including motorcycles
        anprFunc.apply(tsanpr, imageDir + "surround.jpg", outputFormat, "vms"); // Recognize multiple license plates with surround detection
        anprFunc.apply(tsanpr, imageDir + "surround.jpg", outputFormat, "dms"); // Recognize multiple surrounding objects (vehicles)
        anprFunc.apply(tsanpr, imageDir + "surround.jpg", outputFormat, "dmsr"); // Recognize multiple surrounding objects and license plates

        // Recognize multiple surrounding objects and license plates within RoI
        anprFunc.apply(tsanpr, imageDir + "surround.jpg", outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700")

        return 0;
    }

    /**
     * Functional interface for ANPR function selection.
     */
    @FunctionalInterface
    interface AnprFunction {
        void apply(TSANPR tsanpr, String imgfile, String outputFormat, String options);
    }

    public static void main(String[] args) {
        // Set UTF-8 encoding for console output
        System.setOut(new PrintStream(System.out, true, StandardCharsets.UTF_8));
        System.setErr(new PrintStream(System.err, true, StandardCharsets.UTF_8));

        System.out.println("Working directory: " + System.getProperty("user.dir"));
        String jniPath = getJniFileName();
        String enginePath = getEngineFileName();

        // Convert to absolute path and print
        File jniFile = new File(jniPath);
        String absJniPath = jniFile.getAbsolutePath();
        System.out.println("JNI wrapper absolute path: " + absJniPath);

        File engineFile = new File(enginePath);
        String absEnginePath = engineFile.getAbsolutePath();
        System.out.println("Engine absolute path: " + absEnginePath);

        // Use try-with-resources to ensure native resources are released safely
        try (TSANPR tsanpr = new TSANPR(absJniPath, absEnginePath)) {
            // Try each country code as needed
            readLicensePlates(tsanpr, "KR");
            // readLicensePlates(tsanpr, "JP");
            // readLicensePlates(tsanpr, "VN");
        }
        // tsanpr.close() is automatically called here
    }
}
