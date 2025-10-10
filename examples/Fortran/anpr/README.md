English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# Fortran example

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Fortran/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Fortran/anpr)

### 1. Copying the Engine Files

_**[Note]** In this example, the engine file is extracted to the examples/bin/ directory to share it with other examples. However, for actual deployment, the engine file is typically copied to the directory where the application's executable file is located._

- For Windows x86 64-bit
  Extract the engine file to the `examples/bin/windows-x86_64` directory
  ```sh
  tar xvf tsanpr*-windows-x86_64.tar.xz
  ```
- For Windows x86 32-bit
  Extract the engine file to the `examples/bin/windows-x86` directory
  ```sh
  tar xvf tsanpr*-windows-x86.tar.xz
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
  └── Fortran
      └── anpr                   # project directory
         ├── src                 # source directory
         │   ├── anpr.f90
         │   └── tsanpr_module.f90
         ├── build               # build output directory (created by make)
         └── Makefile
  ```

### 2. Prerequisites

1. Install Fortran compiler and build tools

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get update
   sudo apt-get install gfortran build-essential
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   # CentOS/RHEL 7
   sudo yum install gcc-gfortran make

   # CentOS/RHEL 8+ / Fedora
   sudo dnf install gcc-gfortran make
   ```

   **Windows (MinGW/MSYS2):**

   ```sh
   # Install MSYS2 first, then:
   pacman -S mingw-w64-x86_64-gcc-fortran mingw-w64-x86_64-make
   ```

2. Verify installation

   ```sh
   gfortran --version
   make --version
   ```

### 3. How to Build

1. Navigate to the Fortran example directory

   ```sh
   cd Fortran/anpr
   ```

2. Build the example

   ```sh
   make all
   ```

3. Clean build artifacts (if needed)

   ```sh
   make clean
   ```

### 4. How to Run

1. Run `anpr` example

   ```sh
   ./build/anpr
   ```

   Or on Windows:

   ```sh
   build/anpr.exe
   ```

### 5. Notes

- This Fortran implementation provides the same functionality as other language examples
- Uses modern Fortran 2008 standard with ISO C binding for interoperability
- Dynamic library loading is handled through system-specific APIs (dlopen on Unix, LoadLibrary on Windows)

- Pixel buffer processing is simplified in this example - a full implementation would require integration with an image processing library
- Cross-platform compilation is supported through preprocessor directives

### 6. Features

- **File-based recognition**: Process image files directly
- **Encoded image processing**: Handle encoded image data (JPEG, PNG, etc.)
- **Pixel buffer processing**: Process raw pixel data (simplified implementation)
- **Multiple output formats**: Support for text, JSON, YAML, XML, and CSV output
- **Multiple recognition modes**: Single plate, multiple plates, vehicle detection, etc.
- **Region of Interest (RoI)**: Process specific areas within images
- **Multi-country support**: Support for different license plate formats (KR, JP, VN, etc.)
- **Cross-platform compatibility**: Windows and Linux support

### 7. API Reference

#### TSANPR Module

The `tsanpr_module` provides the following types and procedures:

**Types:**

- `tsanpr_handle`: Handle for TSANPR library instance

**Initialization:**

- `tsanpr_init(tsanpr, library_path, status)`: Initialize TSANPR with library path
- `tsanpr_cleanup(tsanpr)`: Clean up TSANPR resources

**Core Functions:**

- `tsanpr_initialize(tsanpr, mode, error_msg, status)`: Initialize the ANPR engine
- `tsanpr_read_file(tsanpr, img_file_name, output_format, options, result, status)`: Process image file
- `tsanpr_read_pixels(tsanpr, pixels, width, height, stride, pixel_format, output_format, options, result, status)`: Process pixel data

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
