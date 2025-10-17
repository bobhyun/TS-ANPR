English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# Zig example

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Zig/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Zig/anpr)

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
  └── Zig
      └── anpr                   # source directory
         ├── build.zig
         ├── src/
         │   ├── main.zig
         │   ├── tsanpr.c
         │   ├── tsanpr.h
         │   └── tsanpr.zig
         └── zig-out/
             └── bin/            # output directory
  ```

### 2. Prerequisites

1. Install Zig (latest stable version recommended)

   **Windows:**

   ```sh
   # Download from https://ziglang.org/download/
   # Or using Scoop
   scoop install zig
   ```

   **Linux:**

   ```sh
   # Download from https://ziglang.org/download/
   # Or using package manager (varies by distribution)
   sudo snap install zig --classic --beta
   ```

2. Verify installation

   ```sh
   zig version
   ```

### 3. How to Run

1. Navigate to the Zig example directory

   ```sh
   cd Zig/anpr
   ```

2. Build and run the example

   ```sh
   # Build and run
   zig build run

   # Build only
   zig build

   # Build with optimizations
   zig build -Doptimize=ReleaseFast

   # Run tests
   zig build test
   ```

### 4. Notes

- This Zig implementation provides the same functionality as other language examples
- Uses Zig's C interoperability to interface with the native TSANPR library
- Zig's compile-time safety and performance make it excellent for system programming
- Cross-platform support for Windows and Linux
- Runtime library loading for better deployment flexibility

### 5. Features

- **File-based recognition**: Process image files directly
- **Encoded image processing**: Handle encoded image data (JPEG, PNG, etc.)
- **Pixel buffer processing**: Process raw pixel data (simplified implementation)
- **Multiple output formats**: Support for text, JSON, YAML, XML, and CSV output
- **Multiple recognition modes**: Single plate, multiple plates, vehicle detection, etc.
- **Region of Interest (RoI)**: Process specific areas within images
- **Multi-country support**: Support for different license plate formats (KR, JP, VN, etc.)

### 6. API Reference

#### TSANPR Functions

The Zig implementation provides the following functions through C interop:

**Initialization:**

- `TSANPR_load(library_path)`: Load the TSANPR library
- `anpr_initialize(mode)`: Initialize the ANPR engine

**Core Functions:**

- `anpr_read_file(img_file_name, output_format, options)`: Process image file
- `anpr_read_pixels(pixels, width, height, stride, pixel_format, output_format, options)`: Process pixel data

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
