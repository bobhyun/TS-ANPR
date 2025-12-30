English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# MATLAB/Octave example

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/MATLAB/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/MATLAB/anpr)

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
  └── MATLAB
      └── anpr                   # project directory
         ├── src                 # source directory
         │   ├── anpr.m          # main example script
         │   ├── TSANPR.m        # TSANPR wrapper class
         │   └── mex             # MEX source directory
         │       ├── tsanpr_mex.c    # MEX source file
         │       └── build_mex.m     # MEX build script
         └── doc.i18n            # translated documentation
  ```

### 2. Prerequisites

#### Option A: MATLAB (Commercial)

1. Install MATLAB (R2018b or later recommended)

   **Windows:**
   - Download and install MATLAB from https://www.mathworks.com/downloads/
   - Requires a valid license

   **Linux:**
   - Download and install MATLAB from https://www.mathworks.com/downloads/
   - Requires a valid license

2. Configure C Compiler
   ```matlab
   mex -setup
   ```

3. Verify installation
   ```matlab
   version
   mex -setup
   ```

#### Option B: GNU Octave (Free, Open Source)

GNU Octave is a free alternative to MATLAB with mostly compatible syntax.

**Windows:**

```cmd
# Using winget
winget install GNU.Octave

# Or download from https://octave.org/download
```

**Linux (Ubuntu/Debian):**

```sh
sudo apt-get update
sudo apt-get install -y octave octave-image liboctave-dev
```

**Linux (Fedora/RHEL):**

```sh
sudo dnf install -y octave octave-image octave-devel
```

Verify installation:
```sh
octave --version
```

### 3. How to Build MEX

The MEX file is automatically built on first run. To build manually:

#### With MATLAB

```matlab
cd examples/MATLAB/anpr/src/mex
build_mex
```

#### With GNU Octave

```sh
cd examples/MATLAB/anpr/src/mex
octave --eval "build_mex"
```

### 4. How to Run

#### With MATLAB

1. Navigate to the source directory
   ```sh
   cd examples/MATLAB/anpr/src
   ```

2. Start MATLAB and run the example
   ```matlab
   % Run the main ANPR example
   anpr
   ```

#### With GNU Octave

**Windows:**

```cmd
cd examples\MATLAB\anpr\src
octave --eval "anpr"
```

**Linux:**

```sh
cd examples/MATLAB/anpr/src
octave --eval "anpr"
```

Or run interactively:
```sh
octave
```
```octave
cd examples/MATLAB/anpr/src
anpr
```

### 5. Interactive Usage

```matlab
% Add paths
addpath('mex');

% Initialize TSANPR
engine_path = '../../../bin/windows-x86_64/tsanpr.dll';  % Windows
% engine_path = '../../../bin/linux-x86_64/libtsanpr.so';  % Linux

tsanpr = TSANPR(engine_path);

% Initialize engine
error_msg = tsanpr.anpr_initialize('text;country=KR');
if ~isempty(error_msg)
    fprintf('Error: %s\n', error_msg);
end

% Process an image
result = tsanpr.anpr_read_file('../../../img/KR/licensePlate.jpg', 'json', '');
fprintf('Result: %s\n', result);
```

### 6. Notes

- This implementation uses MEX files for native library integration
- MEX files provide consistent interface for both MATLAB and Octave
- The MEX file is automatically built on first run if not present
- Cross-platform support for Windows and Linux

### 7. Features

- **File-based recognition**: Process image files directly
- **Encoded image processing**: Handle encoded image data (JPEG, PNG, etc.)
- **Pixel buffer processing**: Process raw pixel data using image processing functions
- **Multiple output formats**: Support for text, JSON, YAML, XML, and CSV output
- **Multiple recognition modes**: Single plate, multiple plates, vehicle detection, etc.
- **Region of Interest (RoI)**: Process specific areas within images
- **Multi-country support**: Support for different license plate formats (KR, JP, VN, etc.)

### 8. API Reference

#### TSANPR Class

**Constructor:**
- `TSANPR(library_path)`: Initialize with native library path

**Core Methods:**
- `anpr_initialize(mode)`: Initialize the ANPR engine
- `anpr_read_file(img_file_name, output_format, options)`: Process image file
- `anpr_read_pixels(pixels, width, height, stride, pixel_format, output_format, options)`: Process pixel data

**Static Methods:**
- `TSANPR.isOctave()`: Check if running in GNU Octave

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

### 9. Troubleshooting

**MEX Build Issues:**
- Ensure C compiler is installed
  - **MATLAB**: Run `mex -setup` to configure
  - **Octave Windows**: MinGW is included with Octave installation
  - **Octave Linux**: Install `liboctave-dev` or `octave-devel`
- Check compiler availability: `mex -setup` (MATLAB) or `mkoctfile --version` (Octave)

**Library Loading Issues:**
- Ensure the TSANPR library path is correct
- Check that all system dependencies are installed
- On Linux, ensure `LD_LIBRARY_PATH` includes the engine directory

**Platform-Specific Issues:**
- **Windows**: Ensure Visual C++ Redistributable is installed
- **Linux**: Install required system libraries and ensure library permissions are correct
