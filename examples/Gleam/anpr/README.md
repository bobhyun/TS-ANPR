English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# Gleam example

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Gleam/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Gleam/anpr)

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
  │   │   ├── tsanpr-*.eon
  │   │   └── tshelper.exe
  │   ├─── windows-x86           # engine directory for Windows (x86)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-*.eon
  │   │   └── tshelper.exe
  │   ├── linux-x86_64           # engine directory for Linux (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-*.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # engine directory for Linux (arm64)
  │       ├── libtsanpr.so
  │       ├── tsanpr-*.eon
  │       └── tshelper
  ├── img                        # image directory
  └── Gleam
      └── anpr                   # project directory
         ├── c_src               # C NIF source files
         │   ├── tsanpr_nif.c    # NIF implementation
         │   └── stb_image.h     # Image decoding library
         ├── src                 # Gleam/Erlang source files
         │   ├── anpr.gleam      # Main module
         │   ├── tsanpr.gleam    # TSANPR API module
         │   ├── tsanpr_ffi.erl  # NIF wrapper module
         │   └── anpr_ffi.erl    # Helper functions
         ├── priv                # Compiled NIF libraries (generated)
         │   └── tsanpr_nif.dll/.so
         ├── gleam.toml          # Gleam project configuration
         ├── manifest.toml       # Gleam dependency manifest
         ├── build_nif.bat       # Windows NIF build script
         ├── build_nif.sh        # Linux NIF build script
         ├── Makefile            # Linux Makefile
         ├── Makefile.win        # Windows Makefile
         └── build               # Gleam build output (generated)
            └── dev/erlang/anpr
               ├── ebin          # Compiled .beam files
               └── priv          # NIF library for runtime
  ```

### 2. Prerequisites

1. Install Gleam (version 1.0 or later recommended)

   **Windows:**

   ```sh
   # Download from https://gleam.run/getting-started/installing/
   # Or using Scoop
   scoop install gleam
   ```

   **Linux:**

   ```sh
   # Download and install manually (x86_64)
   cd /tmp
   wget https://github.com/gleam-lang/gleam/releases/download/v1.14.0/gleam-v1.14.0-x86_64-unknown-linux-musl.tar.gz
   tar xzf gleam-v1.14.0-x86_64-unknown-linux-musl.tar.gz
   sudo mv gleam /usr/local/bin/

   # For ARM64 (aarch64)
   wget https://github.com/gleam-lang/gleam/releases/download/v1.14.0/gleam-v1.14.0-aarch64-unknown-linux-musl.tar.gz
   tar xzf gleam-v1.14.0-aarch64-unknown-linux-musl.tar.gz
   sudo mv gleam /usr/local/bin/
   ```

2. Install Erlang/OTP (version 24 or later recommended)

   **Windows:**

   ```sh
   # Download from https://www.erlang.org/downloads
   # Or using Chocolatey
   choco install erlang
   ```

   **Linux:**

   ```sh
   # Ubuntu/Debian
   sudo apt-get update
   sudo apt-get install -y erlang

   # Oracle Linux / RHEL / CentOS (8/9+)
   sudo dnf install -y erlang || sudo yum install -y erlang
   ```

3. Install C compiler (required for building NIF)

   **Windows:**
   - Install Visual Studio with C++ development tools, or
   - Install MinGW-w64: `choco install mingw`

   **Linux:**
   ```sh
   # Ubuntu/Debian
   sudo apt-get install build-essential

   # Fedora/CentOS
   sudo yum groupinstall "Development Tools"
   ```

4. Verify installation

   ```sh
   gleam --version
   erl -version
   gcc --version  # or cl.exe on Windows with Visual Studio
   ```

### 3. Build and Run

1. Navigate to the Gleam example directory

   ```sh
   cd examples/Gleam/anpr
   ```

2. Build the NIF (Native Implemented Function) library

   **Windows:**

   Open "x64 Native Tools Command Prompt for VS" (or run vcvars64.bat), then:
   ```cmd
   build_nif.bat
   ```

   This will:
   - Automatically detect your Erlang installation
   - Compile `c_src/tsanpr_nif.c` using MSVC
   - Create `priv/tsanpr_nif.dll`

   **Linux:**

   ```sh
   chmod +x build_nif.sh
   ./build_nif.sh
   # Or use make
   make priv/tsanpr_nif.so
   ```

   This will:
   - Compile `c_src/tsanpr_nif.c` using gcc
   - Create `priv/tsanpr_nif.so` with proper linking to libdl

3. Build and run the Gleam application

   ```sh
   # Download dependencies
   gleam deps download

   # Build and run
   gleam run
   ```

   Or build separately:
   ```sh
   gleam build
   gleam run
   ```

### 4. Notes

- This Gleam implementation provides the same functionality as other language examples
- Uses Gleam's Erlang FFI with C NIFs to interface with the native TSANPR library
- Gleam's type safety and functional programming paradigm provide excellent reliability
- Runs on the Erlang Virtual Machine (BEAM) for excellent concurrency and fault tolerance
- Cross-platform support for Windows and Linux
- The project follows Gleam's standard structure with Erlang interop for NIF calls

### 5. Features

- **File-based recognition**: Process image files directly
- **Encoded image processing**: Handle encoded image data (JPEG, PNG, etc.)
- **Pixel buffer processing**: Process raw pixel data (simplified implementation)
- **Multiple output formats**: Support for text, JSON, YAML, XML, and CSV output
- **Multiple recognition modes**: Single plate, multiple plates, vehicle detection, etc.
- **Region of Interest (RoI)**: Process specific areas within images
- **Multi-country support**: Support for different license plate formats (KR, JP, VN, etc.)

### 6. API Reference

#### tsanpr Module

The `tsanpr` module provides the following functions:

**Initialization:**

- `tsanpr.new(library_path: String) -> Result(TSANPR, String)`: Create TSANPR instance

**Core Functions:**

- `tsanpr.initialize(tsanpr: TSANPR, options: String) -> Result(String, String)`: Initialize the ANPR engine
- `tsanpr.read_file(tsanpr: TSANPR, filepath: String, output_format: String, options: String) -> Result(String, String)`: Process image file
- `tsanpr.read_pixels_binary(tsanpr: TSANPR, pixels: Dynamic, width: Int, height: Int, stride: Int, pixel_format: String, output_format: String, options: String) -> Result(String, String)`: Process pixel data

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

**NIF Issues:**

- Ensure the NIF is built before running (`build_nif.bat` or `build_nif.sh`)
- The NIF must be in the `priv/` directory
- Ensure Erlang development headers are installed

**Compilation Issues:**

- On Windows, run from Visual Studio Developer Command Prompt
- On Linux, ensure `gcc` and development headers are installed
- Check for syntax errors using `gleam check`

**Platform-Specific Issues:**

- **Windows**: Ensure Visual C++ Redistributable is installed
- **Linux**: Install required system libraries (`sudo apt-get install build-essential`)
