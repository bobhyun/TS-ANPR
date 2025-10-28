English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# D example

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/D/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/D/anpr)

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
  └── D
      └── anpr                   # source directory
         ├── anpr.d
         ├── dub.json
         └── tsanpr.d
  ```

### 2. Prerequisites

1. Install D compiler (DMD, LDC, or GDC)

   **Windows:**

   ```sh
   # Download and install DMD from https://dlang.org/download.html
   # Or use chocolatey
   choco install dmd
   ```

   **Ubuntu / Debian:**

   ```sh
   # Install using snap
   sudo snap install dmd --classic
   sudo snap install dub --classic

   # Or install using apt
   sudo wget https://netcologne.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list
   sudo apt-get update && sudo apt-get -y --allow-unauthenticated install --reinstall d-apt-keyring
   sudo apt-get update && sudo apt-get install dmd-compiler dub
   ```

   **Oracle Linux / RedHat (RHEL) / CentOS:**

   ```sh
   # Download and install manually from https://dlang.org/download.html
   curl -fsS https://dlang.org/install.sh | bash -s dmd
   source ~/dlang/dmd-*/activate
   ```

2. Verify installation

   ```sh
   dmd --version
   ```

### 3. How to Run

1. Navigate to the D example directory

   ```sh
   cd D/anpr
   ```

2. Compile and run the example

   **Using DUB (recommended):**

   ```sh
   # Build and run
   dub run

   # Build only
   dub build

   # Build release version
   dub build --build=release

   # Run
   ./bin/anpr
   ```

### 4. Notes

- This D implementation provides the same functionality as other language examples
- Uses D's excellent C interoperability to interface with the native TSANPR library
- D's systems programming capabilities and modern language features provide both performance and safety
- Cross-platform support for Windows and Linux
- D's built-in memory management and garbage collection simplify development

### 5. Features

- **File-based recognition**: Process image files directly
- **Encoded image processing**: Handle encoded image data (JPEG, PNG, etc.)
- **Pixel buffer processing**: Process raw pixel data (simplified implementation)
- **Multiple output formats**: Support for text, JSON, YAML, XML, and CSV output
- **Multiple recognition modes**: Single plate, multiple plates, vehicle detection, etc.
- **Region of Interest (RoI)**: Process specific areas within images
- **Multi-country support**: Support for different license plate formats (KR, JP, VN, etc.)

### 6. API Reference

#### TSAnpr Class

The `TSAnpr` class provides the following methods:

**Constructor:**

- `this(string libraryPath)`: Initialize with native library path

**Core Methods:**

- `string anprInitialize(string enginePath)`: Initialize the ANPR engine
- `string anprReadFile(string imagePath, string options, string outputFormat)`: Process image file
- `string anprReadPixels(ubyte[] pixels, int width, int height, int channels, string options, string outputFormat)`: Process pixel data

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

### 7. Troubleshooting

**Library Loading Issues:**

- Ensure the TSANPR library path is correct
- Check that all system dependencies are installed
- Verify library permissions are correct

**Compilation Issues:**

- Ensure D compiler is properly installed
- Use appropriate compiler flags for your target platform
- Consider using DUB package manager for complex projects

**Platform-Specific Issues:**

- **Windows**: Ensure Visual C++ Redistributable is installed
- **Linux**: Install required system libraries and ensure library permissions are correct
