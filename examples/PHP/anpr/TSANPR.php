<?php
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

/**
 * TSANPR wrapper class for PHP.
 * Provides interface to the native TSANPR library using FFI.
 */
class TSANPR {
    private FFI $ffi;
    
    /**
     * Initialize TSANPR with the given library path.
     * 
     * @param string $libraryPath Path to the TSANPR library
     * @throws Exception If library loading fails
     */
    public function __construct(string $libraryPath) {
        if (!extension_loaded('ffi')) {
            throw new Exception('FFI extension is not loaded. Please enable FFI in php.ini');
        }
        
        if (!file_exists($libraryPath)) {
            throw new Exception("Library file not found: {$libraryPath}");
        }
        
        try {
            // Define C function signatures
            $this->ffi = FFI::cdef("
                char* anpr_initialize(const char* mode);
                char* anpr_read_file(const char* img_file_name, const char* output_format, const char* options);
                char* anpr_read_pixels(void* pixels, uint64_t width, uint64_t height, int64_t stride, 
                                     const char* pixel_format, const char* output_format, const char* options);
            ", $libraryPath);
            
        } catch (FFI\Exception $e) {
            throw new Exception("Failed to load native library: {$libraryPath}\n" . $e->getMessage());
        }
    }
    
    /**
     * Initialize the ANPR engine with the specified mode.
     * 
     * @param string $mode Initialization mode string (e.g., "text;country=KR")
     * @return string Error message if initialization failed, empty string if successful
     */
    public function anprInitialize(string $mode): string {
        try {
            $result = $this->ffi->anpr_initialize($mode);
            return $result ? FFI::string($result) : '';
        } catch (FFI\Exception $e) {
            return "Function call failed: " . $e->getMessage();
        }
    }
    
    /**
     * Read and process an image file.
     * 
     * @param string $imgFileName Path to the image file
     * @param string $outputFormat Output format (text, json, yaml, xml, csv)
     * @param string $options Processing options
     * @return string Recognition result as string
     */
    public function anprReadFile(string $imgFileName, string $outputFormat, string $options): string {
        try {
            $result = $this->ffi->anpr_read_file($imgFileName, $outputFormat, $options);
            return $result ? FFI::string($result) : '';
        } catch (FFI\Exception $e) {
            return "Function call failed: " . $e->getMessage();
        }
    }
    
    /**
     * Process pixel data directly.
     * 
     * @param string $pixels Pixel data as binary string
     * @param int $width Image width
     * @param int $height Image height
     * @param int $stride Row stride in bytes
     * @param string $pixelFormat Pixel format (BGR, BGRA, GRAY, etc.)
     * @param string $outputFormat Output format (text, json, yaml, xml, csv)
     * @param string $options Processing options
     * @return string Recognition result as string
     */
    public function anprReadPixels(
        string $pixels, 
        int $width, 
        int $height, 
        int $stride,
        string $pixelFormat, 
        string $outputFormat, 
        string $options
    ): string {
        try {
            // Create FFI buffer from string data
            $pixelBuffer = FFI::new("char[" . strlen($pixels) . "]");
            FFI::memcpy($pixelBuffer, $pixels, strlen($pixels));
            
            $result = $this->ffi->anpr_read_pixels(
                FFI::addr($pixelBuffer),
                $width,
                $height,
                $stride,
                $pixelFormat,
                $outputFormat,
                $options
            );
            
            return $result ? FFI::string($result) : '';
        } catch (FFI\Exception $e) {
            return "Function call failed: " . $e->getMessage();
        }
    }
}
?>