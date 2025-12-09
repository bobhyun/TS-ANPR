English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# Elixir example

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Elixir/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Elixir/anpr)

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
  └── Elixir
      └── anpr                   # project directory
         ├── c_src               # C NIF source files
         │   └── tsanpr_nif.c
         ├── lib                 # Elixir source files
         │   ├── anpr.ex         # Main module
         │   └── tsanpr.ex       # NIF wrapper module
         ├── priv                # Compiled NIF libraries
         │   └── tsanpr_nif.dll/.so
         ├── mix.exs             # Mix project configuration
         ├── Makefile            # Build configuration (Linux)
         ├── Makefile.win        # Build configuration (Windows, nmake)
         ├── build_nif.bat       # Build script (Windows)
         └── _build              # Mix build output (generated)
            └── dev
               └── lib
                  └── anpr
                     ├── ebin    # Compiled .beam files
                     └── priv    # NIF libraries
  ```

### 2. Prerequisites

1. Install Elixir (version 1.14 or later recommended)

   **Windows:**

   ```sh
   # Using Chocolatey
   choco install elixir

   # Or download from https://elixir-lang.org/install.html#windows
   ```

   **Linux:**

   ```sh
   # Ubuntu/Debian
   sudo apt-get install elixir

   # Oracle Linux / RHEL / CentOS
   sudo dnf install elixir || sudo yum install elixir
   # Tip: If elixir is not found, enable EPEL or use Erlang Solutions repo
   ```

2. Install C compiler (required for building NIF)

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

3. Verify installation

   ```sh
   elixir --version
   mix --version
   gcc --version  # or cl.exe on Windows with Visual Studio
   ```

### 3. Build and Run

1. Navigate to the Elixir example directory

   ```sh
   cd examples/Elixir/anpr
   ```

2. Build the NIF (Native Implemented Function) library

**Windows:**

Open "x64 Native Tools Command Prompt for VS 2022" (or run `vcvars64.bat`) so `cl`/`link` are on PATH, then:
   ```cmd
   build_nif.bat
   ```

   This will:
   - Automatically detect your Erlang installation
   - Compile `c_src/tsanpr_nif.c` using MSVC
   - Create `priv/tsanpr_nif.dll`

Alternatively, if `mix compile` invokes `nmake` via `elixir_make`, it will use `Makefile.win`, which delegates to `build_nif.bat` to avoid quoting issues on Windows.

   **Linux:**

   ```sh
   make priv/tsanpr_nif.so
   ```

   This will:
   - Compile `c_src/tsanpr_nif.c` using gcc
   - Create `priv/tsanpr_nif.so` with proper linking to libdl

3. Install dependencies and compile the Elixir application

   ```sh
   mix deps.get
   mix compile
   ```

   This will:
   - Download dependencies (elixir_make)
   - Compile Elixir modules (`anpr.ex`, `tsanpr.ex`)
   - Copy the NIF library to `_build/dev/lib/anpr/priv/`

4. Run the application

   ```sh
   # Using Mix
   mix run -e "ANPR.main()"

   # Or using iex (interactive)
   iex -S mix
   iex> ANPR.main()
   ```

### 4. Notes

- This Elixir implementation provides the same functionality as other language examples
- Uses Elixir's NIFs (Native Implemented Functions) to interface with the native TSANPR library
- Elixir's fault-tolerant design and actor model make it excellent for distributed systems
- Cross-platform support for Windows and Linux
- Built using Mix, the standard Elixir build tool
- The project follows Mix-standardized structure for better maintainability
- Note: This is a simplified NIF implementation for demonstration purposes

Additional implementation notes:
- On Windows, `ANPR.get_engine_file_name/0` selects `examples/bin/windows-x86_64/tsanpr.dll` on 64‑bit BEAM (`:erlang.system_info(:wordsize) == 8`), otherwise uses `windows-x86`.
- The NIF module is registered as `Elixir.TSANPR` in `c_src/tsanpr_nif.c` and loaded by basename (`tsanpr_nif`) in `lib/tsanpr.ex` so the correct platform extension is picked automatically.

### 5. Features

- **File-based recognition**: Process image files directly
- **Encoded image processing**: Handle encoded image data (JPEG, PNG, etc.)
- **Pixel buffer processing**: Process raw pixel data (simplified implementation)
- **Multiple output formats**: Support for text, JSON, YAML, XML, and CSV output
- **Multiple recognition modes**: Single plate, multiple plates, vehicle detection, etc.
- **Region of Interest (RoI)**: Process specific areas within images
- **Multi-country support**: Support for different license plate formats (KR, JP, VN, etc.)

### 6. API Reference

#### TSANPR Module

The `TSANPR` module provides the following functions:

**Initialization:**

- `TSANPR.load(library_path)`: Initialize with native library path

**Core Functions:**

- `TSANPR.anpr_initialize(tsanpr, mode)`: Initialize the ANPR engine
- `TSANPR.anpr_read_file(tsanpr, img_file_name, output_format, options)`: Process image file
- `TSANPR.anpr_read_pixels(tsanpr, pixels, width, height, stride, pixel_format, output_format, options)`: Process pixel data

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

**Platform-Specific Issues:**

- **Windows**: Ensure Visual C++ Redistributable is installed
- **Linux**: Install required system libraries and ensure library permissions are correct
