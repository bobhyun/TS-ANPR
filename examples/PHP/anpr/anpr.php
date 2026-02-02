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

require_once 'TSANPR.php';

define('EXAMPLES_BASE_DIR', realpath(__DIR__ . '/../..'));

/**
 * Generate engine filename depending on platform and architecture.
 * 
 * @return string Engine file path
 */
function getEngineFileName(): string {
    $os = PHP_OS_FAMILY;
    $arch = php_uname('m');
    
    if ($os === 'Windows') {
        if ($arch === 'AMD64' || $arch === 'x86_64') {
            return EXAMPLES_BASE_DIR . '/bin/windows-x86_64/tsanpr.dll';
        } elseif ($arch === 'x86' || $arch === 'i386') {
            return EXAMPLES_BASE_DIR . '/bin/windows-x86/tsanpr.dll';
        }
    } elseif ($os === 'Linux') {
        if ($arch === 'x86_64' || $arch === 'amd64') {
            return EXAMPLES_BASE_DIR . '/bin/linux-x86_64/libtsanpr.so';
        } elseif ($arch === 'aarch64') {
            return EXAMPLES_BASE_DIR . '/bin/linux-aarch64/libtsanpr.so';
        }
    }
    
    return '';
}

/**
 * Read an image file and call anpr_read_file.
 * 
 * @param TSANPR $tsanpr TSANPR instance
 * @param string $imgfile Image file path
 * @param string $outputFormat Output format
 * @param string $options Processing options
 */
function readImageFile(TSANPR $tsanpr, string $imgfile, string $outputFormat, string $options): void {
    echo "{$imgfile} (outputFormat=\"{$outputFormat}\", options=\"{$options}\") => ";
    $result = $tsanpr->anprReadFile($imgfile, $outputFormat, $options);
    echo $result . PHP_EOL;
}

/**
 * Read an encoded image file as bytes and call tsanpr->anprReadPixels with 'encoded' pixel format.
 * 
 * @param TSANPR $tsanpr TSANPR instance
 * @param string $imgfile Image file path
 * @param string $outputFormat Output format
 * @param string $options Processing options
 */
function readEncodedImage(TSANPR $tsanpr, string $imgfile, string $outputFormat, string $options): void {
    echo "{$imgfile} (outputFormat=\"{$outputFormat}\", options=\"{$options}\") => ";
    
    try {
        if (!file_exists($imgfile)) {
            echo "File does not exist" . PHP_EOL;
            return;
        }
        
        $encodedImg = file_get_contents($imgfile);
        if ($encodedImg === false) {
            echo "Failed to read file" . PHP_EOL;
            return;
        }
        
        $result = $tsanpr->anprReadPixels(
            $encodedImg,
            strlen($encodedImg),
            0,
            0,
            'encoded',
            $outputFormat,
            $options
        );
        echo $result . PHP_EOL;
        
    } catch (Exception $e) {
        echo "ERROR: Exception - " . $e->getMessage() . PHP_EOL;
    }
}

/**
 * Determine pixel format string based on image channels.
 * 
 * @param int $channels Number of color channels
 * @return string Pixel format string
 */
function getPixelFormat(int $channels): string {
    switch ($channels) {
        case 1:
            return 'GRAY';
        case 2:
            return 'BGR565';  // or "BGR555"
        case 3:
            return 'BGR';
        case 4:
            return 'BGRA';
        default:
            return '';
    }
}

/**
 * Use the pixel buffer-based ANPR function.
 * Uses Imagick extension to get raw pixel data directly without conversion.
 *
 * @param TSANPR $tsanpr TSANPR instance
 * @param string $imgfile Image file path
 * @param string $outputFormat Output format
 * @param string $options Processing options
 */
function readPixelBuffer(TSANPR $tsanpr, string $imgfile, string $outputFormat, string $options): void {
    echo "{$imgfile} (outputFormat=\"{$outputFormat}\", options=\"{$options}\") => ";

    try {
        if (!extension_loaded('imagick')) {
            echo "Imagick extension not available for pixel buffer processing" . PHP_EOL;
            return;
        }

        $imagick = new Imagick($imgfile);
        $width = $imagick->getImageWidth();
        $height = $imagick->getImageHeight();

        // Get raw pixel data directly without pixel rearrangement
        $imagick->setImageDepth(8);
        $imagick->setImageFormat('rgb');
        $pixelData = $imagick->getImageBlob();
        $pixelFormat = 'RGB';
        $channels = 3;

        $imagick->destroy();

        $stride = $width * $channels;
        $result = $tsanpr->anprReadPixels(
            $pixelData,
            $width,
            $height,
            $stride,
            $pixelFormat,
            $outputFormat,
            $options
        );
        echo $result . PHP_EOL;

    } catch (Exception $e) {
        echo "ERROR: Exception - " . $e->getMessage() . PHP_EOL;
    }
}

/**
 * NOTICE:
 * anpr_initialize should be called only once after library load.
 * Therefore, it is not possible to change the country code after anpr_initialize has been called.
 * While using the free trial license, you can try all languages.
 * When you purchase a commercial license, you can only use the selected language.
 * 
 * @param TSANPR $tsanpr TSANPR instance
 * @param string $countryCode Country code (KR, JP, VN, etc.)
 */
function readLicensePlates(TSANPR $tsanpr, string $countryCode): void {
    $error = $tsanpr->anprInitialize("text;country={$countryCode}");
    if (!empty($error)) {
        echo "anpr_initialize() failed: {$error}" . PHP_EOL;
        return;
    }
    
    $imageDir = EXAMPLES_BASE_DIR . "/img/{$countryCode}";
    
    // TODO: Try each function as needed
    // $anprFunc = 'readImageFile';
    // $anprFunc = 'readEncodedImage';
    $anprFunc = 'readPixelBuffer';
    
    // TODO: Try each output format as needed
    $outputFormat = 'text';
    // $outputFormat = 'json';
    // $outputFormat = 'yaml';
    // $outputFormat = 'xml';
    // $outputFormat = 'csv';
    
    $anprFunc($tsanpr, "{$imageDir}/licensePlate.jpg", $outputFormat, "");   // Single license plate recognition (default)
    $anprFunc($tsanpr, "{$imageDir}/multiple.jpg", $outputFormat, "vm");     // Recognize multiple license plates attached to vehicles
    $anprFunc($tsanpr, "{$imageDir}/multiple.jpg", $outputFormat, "vmb");    // Recognize multiple license plates attached to vehicles (including motorcycles)
    $anprFunc($tsanpr, "{$imageDir}/surround.jpg", $outputFormat, "vms");    // Recognize multiple license plates attached to vehicles with surround detection
    $anprFunc($tsanpr, "{$imageDir}/surround.jpg", $outputFormat, "dms");    // Recognize multiple surrounding objects (vehicles)
    $anprFunc($tsanpr, "{$imageDir}/surround.jpg", $outputFormat, "dmsr");   // Recognize multiple surrounding objects (vehicles) and license plates
    
    // Recognize multiple surrounding objects and license plates within RoI
    $anprFunc($tsanpr, "{$imageDir}/surround.jpg", $outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700");
}

/**
 * Main function
 */
function main(): void {
    $engineFileName = getEngineFileName();
    if (empty($engineFileName) || !file_exists($engineFileName)) {
        echo "Unsupported operating system or engine not found" . PHP_EOL;
        return;
    }
    
    try {
        $tsanpr = new TSANPR($engineFileName);
        
        // TODO: Try each country code as needed
        readLicensePlates($tsanpr, "KR");
        // readLicensePlates($tsanpr, "JP");
        // readLicensePlates($tsanpr, "VN");
        
    } catch (Exception $e) {
        echo "TSANPR initialization failed: " . $e->getMessage() . PHP_EOL;
    }
}

// Run the main function
main();
?>