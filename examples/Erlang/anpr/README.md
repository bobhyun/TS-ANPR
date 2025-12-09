English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# Erlang example

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Erlang/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Erlang/anpr)

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
  └── Erlang
      └── anpr                   # project directory
         ├── c_src               # C NIF source files
         │   └── tsanpr_nif.c
         ├── src                 # Erlang source files
         │   ├── anpr_app.erl    # Application behavior
         │   ├── anpr_sup.erl    # Supervisor behavior
         │   ├── anpr.erl        # Main module
         │   ├── tsanpr.erl      # NIF wrapper module
         │   └── anpr.app.src    # Application resource file
         ├── priv                # Compiled NIF libraries
         │   └── tsanpr_nif.dll/.so
         ├── rebar.config        # rebar3 configuration
         └── _build              # rebar3 build output (generated)
            └── default
               └── lib
                  └── anpr
                     ├── ebin    # Compiled .beam files
                     └── priv    # NIF libraries
  ```

### 2. Prerequisites

1. Install Erlang/OTP (version 24 or later recommended)

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
   # Tip: If erlang is not found, enable EPEL or use Erlang Solutions repo:
   # https://www.erlang.org/downloads

   # Or using kerl (recommended)
   curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
   chmod a+x kerl
   ./kerl build 24.3 24.3
   ./kerl install 24.3 ~/erlang/24.3
   # Activate this Erlang in current shell
   . ~/erlang/24.3/activate
   # (To deactivate later) run: deactivate
   ```

2. Install rebar3

   **Windows:**

   Download the latest rebar3 escript from GitHub:
   ```powershell
   # Download latest rebar3 (compatible with Erlang/OTP 28+)
   Invoke-WebRequest -Uri "https://github.com/erlang/rebar3/releases/latest/download/rebar3" -OutFile "rebar3"
   
   # Place rebar3 in your project directory or move to a directory in your PATH
   ```

   **Note:** On Windows, rebar3 must be run using `escript rebar3 <command>` (not just `rebar3 <command>`).

   **Linux:**

   ```sh
   # Ubuntu/Debian
   sudo apt-get install rebar3
   
   # Or download latest from GitHub
   wget https://github.com/erlang/rebar3/releases/latest/download/rebar3
   chmod +x rebar3
   sudo mv rebar3 /usr/local/bin/
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
   erl -version
   rebar3 version
   gcc --version  # or cl.exe on Windows with Visual Studio
   ```

### 3. Build and Run

1. Navigate to the Erlang example directory

   ```sh
   cd examples/Erlang/anpr
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
   make priv/tsanpr_nif.so
   ```

   This will:
   - Compile `c_src/tsanpr_nif.c` using gcc
   - Create `priv/tsanpr_nif.so` with proper linking to libdl

3. Build the Erlang application

   **Windows (use escript command):**
   ```cmd
   escript rebar3 compile
   ```

   **Linux:**
   ```sh
   rebar3 compile
   ```

   This will:
   - Compile Erlang modules (`anpr.erl`, `tsanpr.erl`) to `_build/default/lib/anpr/ebin/`
   - Copy the NIF library from `priv/` to `_build/default/lib/anpr/priv/`

4. Run the application

   ```sh
   # Non-interactive mode
   erl -pa _build/default/lib/anpr/ebin -noshell -eval "anpr:main()" -s init stop

   # Interactive mode
   erl -pa _build/default/lib/anpr/ebin
   1> anpr:main().
   ```

### 4. Notes

- This Erlang implementation provides the same functionality as other language examples
- Uses Erlang's NIFs (Native Implemented Functions) to interface with the native TSANPR library
- Erlang's fault-tolerant design and lightweight processes make it excellent for concurrent systems
- Cross-platform support for Windows and Linux
- Built using rebar3, the standard Erlang build tool
- The project follows rebar3-standardized structure for better maintainability
- Note: This is a simplified NIF implementation for demonstration purposes

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

- `tsanpr:new(LibraryPath)`: Initialize with native library path

**Core Functions:**

- `tsanpr:anpr_initialize(TSANPR, Mode)`: Initialize the ANPR engine
- `tsanpr:anpr_read_file(TSANPR, ImgFileName, OutputFormat, Options)`: Process image file
- `tsanpr:anpr_read_pixels(TSANPR, Pixels, Width, Height, Stride, PixelFormat, OutputFormat, Options)`: Process pixel data

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

- This example uses a simplified NIF approach for demonstration
- For production use, you would need to implement proper C NIFs
- Ensure Erlang development headers are installed

**Compilation Issues:**

- Ensure Erlang compiler is properly installed
- Use `erlc` to compile modules before running
- Check for syntax errors in .erl files

**Platform-Specific Issues:**

- **Windows**: Ensure Visual C++ Redistributable is installed
- **Linux**: Install required system libraries and ensure library permissions are correct
