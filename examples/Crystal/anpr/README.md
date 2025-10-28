English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# Crystal example

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Crystal/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Crystal/anpr)

### 1. Copying the Engine Files

_**[Note]** In this example, the engine file is extracted to the examples/bin/ directory to share it with other examples. However, for actual deployment, the engine file is typically copied to the directory where the application's executable file is located._

- For Windows x86 64-bit
  Extract the engine file to the `examples/bin/windows-x86_64` directory
  ```sh
  7z x tsanpr*-windows-x86_64.7z
  ```
- For Windows x86 32-bit
  Extract the engine file to the `examples/bin/windows-x86` directory
  ```sh
  7z x tsanpr*-windows-x86.7z
  ```
- For Linux x86 64-bit
  Extract the engine file to the `examples/bin/linux-x86_64` directory
  ```sh
  tar xvf tsanpr-linux-x86_64.tar.xz
  ```
- For Linux arm 64-bit
  Extract the engine file to the `examples/bin/linux-aarch64` directory
  ```sh
  tar xvf tsanpr-linux-aarch64.tar.xz
  ```
- Directory structure
  ```sh
  examples
  ├── bin
  │   ├─── windows-x86_64        # engine directory for Windows (x86_64)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86           # engine directory for Windows (x86)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64           # engine directory for Linux (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # engine directory for Linux (arm64)
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # image directory
  └── Crystal
      └── anpr                   # source directory
         ├── anpr.cr
         ├── shard.yml
         └── tsanpr.cr
  ```

### 2. Prerequisites

1. Install Crystal (version 1.0.0 or later recommended)

   **Windows:**

   ```sh
   # Using Scoop
   scoop install crystal

   # Or download from https://crystal-lang.org/install/on_windows/
   ```

   **Linux:**

   ```sh
   # Ubuntu/Debian
   curl -fsSL https://crystal-lang.org/install.sh | sudo bash

   # Or using package manager
   sudo apt-get install crystal
   ```

2. Verify installation

   ```sh
   crystal --version
   ```

### 3. How to Run

1. Navigate to the Crystal example directory

   ```sh
   cd Crystal/anpr
   ```

2. Install dependencies

   ```sh
   # Install required shards (StumpyPNG and StumpyJPEG for image decoding)
   shards install
   ```

   **Windows Note:** Shards requires symlinks. If you encounter a symlink error, you have two options:

   - **Option 1 (Recommended):** Enable Developer Mode in Windows Settings
     - Go to Settings → Privacy & Security → For developers → Developer Mode → ON
     - See: https://learn.microsoft.com/en-us/windows/apps/get-started/enable-your-device-for-development

   - **Option 2:** Run PowerShell or Command Prompt as Administrator
     ```powershell
     # Run as Administrator
     shards install
     ```

3. Run the example

   **Using Shards (recommended):**

   ```sh
   # Build and run
   shards build
   ./bin/anpr

   # Build with optimizations
   shards build --release
   ```

   **Direct compilation:**

   ```sh
   # Run directly
   crystal run anpr.cr

   # Or compile and run
   crystal build anpr.cr
   ./anpr

   # Compile with optimizations
   crystal build --release anpr.cr
   ```

### 4. Notes

- This Crystal implementation provides the same functionality as other language examples
- Uses Crystal's `lib` binding to interface with the native TSANPR library
- Crystal's Ruby-like syntax with C-like performance makes it ideal for system programming
- Cross-platform support for Windows and Linux
- Memory-safe with garbage collection and compile-time null checks

### 5. Features

- **File-based recognition**: Process image files directly using `anpr_read_file`
- **Encoded image processing**: Handle encoded image data (JPEG, PNG, etc.) as byte arrays
- **Pixel buffer processing**: Process decoded pixel data in BGR format using StumpyPNG and StumpyJPEG libraries
- **Multiple output formats**: Support for text, JSON, YAML, XML, and CSV output
- **Multiple recognition modes**: Single plate, multiple plates, vehicle detection, etc.
- **Region of Interest (RoI)**: Process specific areas within images
- **Multi-country support**: Support for different license plate formats (KR, JP, VN, etc.)

### 6. Image Processing Methods

This example demonstrates three different ways to process images:

1. **read_image_file**: Direct file processing using `anpr_read_file` (fastest method)
2. **read_encoded_image**: Pass encoded image bytes with pixel format "encoded"
3. **read_pixel_buffer**: Decode image to raw RGB pixel data using StumpyPNG/StumpyJPEG, then pass to `anpr_read_pixels`
   - PNG files: Decoded using StumpyPNG
   - JPEG files: Decoded using StumpyJPEG, with automatic fallback to encoded format if decoding fails

To switch between methods, modify the `anpr_func` variable in the code:

```crystal
# Choose one of the following:
anpr_func = ->read_image_file(TSANPR, String, String, String)
# anpr_func = ->read_encoded_image(TSANPR, String, String, String)
# anpr_func = ->read_pixel_buffer(TSANPR, String, String, String)
```

**Note**: The `read_pixel_buffer` function automatically handles JPEG decoding failures by falling back to encoded format, ensuring robust operation across different JPEG variants.

### 7. API Reference

#### TSANPR Class

The `TSANPR` class provides the following methods:

**Constructor:**

- `TSANPR.new(library_path : String)`: Initialize with native library path

**Core Methods:**

- `anpr_initialize(mode : String) : String`: Initialize the ANPR engine
- `anpr_read_file(img_file_name : String, output_format : String, options : String) : String`: Process image file
- `anpr_read_pixels(pixels : UInt8*, width : UInt64, height : UInt64, stride : Int64, pixel_format : String, output_format : String, options : String) : String`: Process pixel data

#### Recognition Options

- `""`: Single license plate recognition (default)
- `"vm"`: Recognize multiple license plates attached to vehicles
- `"vmb"`: Recognize multiple license plates attached to vehicles (including motorcycles)
- `"vms"`: Recognize multiple license plates attached to vehicles with surround detection
- `"dms"`: Recognize multiple surrounding objects (vehicles)
- `"dmsr"`: Recognize multiple surrounding objects (vehicles) and license plates
- `"dmsri<coordinates>"`: Recognize within Region of Interest

#### Output Formats

- `"text"`: Plain text output
- `"json"`: JSON format output
- `"yaml"`: YAML format output
- `"xml"`: XML format output
- `"csv"`: CSV format output
