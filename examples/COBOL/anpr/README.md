English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# COBOL example

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/COBOL/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/COBOL/anpr)

This example demonstrates how to integrate the TS-ANPR engine with COBOL applications using a C wrapper library.

## Architecture

The COBOL example uses a layered architecture:

```
COBOL Application (anpr.cbl)
         ↓
C Wrapper Library (libtsanpr_cobol.so/.dll)
         ↓
TS-ANPR Engine (libtsanpr.so/tsanpr.dll)
```

- **COBOL Application**: Main program that calls the C wrapper
- **C Wrapper**: Provides COBOL-friendly interface to the TS-ANPR engine
- **TS-ANPR Engine**: Deep learning-based license plate recognition engine

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
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- For Linux arm 64-bit
  Extract the engine file to the `examples/bin/linux-aarch64` directory
  ```sh
  tar xvf tsanpr*-linux-aarch64.tar.xz
  ```
- Directory structure
  ```
  examples
  ├── bin
  │   ├── windows-x86_64        # engine directory for Windows (x86_64)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── windows-x86           # engine directory for Windows (x86)
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
  └── COBOL
      └── anpr                   # source directory
         ├── src
         │   ├── c               # C wrapper source files
         │   │   ├── tsanpr_cobol.c
         │   │   └── tsanpr_cobol.h
         │   └── cobol           # COBOL source files
         │       └── anpr.cbl
         ├── bin                 # build output directory
         ├── compile.bat         # Windows build script
         ├── compile.sh          # Linux build script
         ├── run.bat             # Windows run script
         ├── run.sh              # Linux run script
         └── Makefile            # GNU Make build file
  ```

### 2. Build and Run

#### 2.1 Windows

1. **GnuCOBOL Installation**

   - Download and install [GnuCOBOL for Windows](https://sourceforge.net/projects/gnucobol/)
   - Or install via [SuperBOL](https://superbol.eu/developers/windows/) (recommended)
   - Add GnuCOBOL bin directory to PATH

2. **GCC Installation**

   - GCC is required to build the C wrapper
   - If you installed SuperBOL, MinGW64 GCC is already included
   - Otherwise, install [MinGW-w64](https://www.mingw-w64.org/)

3. **How to Build**

   ```cmd
   compile.bat
   ```

   Or using Make:

   ```cmd
   make
   ```

4. **How to Run**

   ```cmd
   run.bat
   ```

   Or using Make:

   ```cmd
   make run
   ```

#### 2.2 Linux

1. **Dependency Installation**

   - Debian / Ubuntu Linux

     ```sh
     sudo apt-get update
     sudo apt-get install gnucobol gcc
     ```

   - Oracle Linux / RedHat (RHEL) / CentOS

     ```sh
     sudo yum install gnucobol gcc
     ```

   - Fedora

     ```sh
     sudo dnf install gnucobol gcc
     ```

2. **How to Build**

   ```sh
   chmod +x compile.sh
   ./compile.sh
   ```

   Or using Make:

   ```sh
   make
   ```

3. **How to Run**

   ```sh
   chmod +x run.sh
   ./run.sh
   ```

   Or using Make:

   ```sh
   make run
   ```

## Implementation Details

### C Wrapper (`src/c/tsanpr_cobol.c`)

The C wrapper provides three main functions:

- `tsanpr_cobol_initialize(options, options_len, result, result_len)`: Initialize the ANPR engine
- `tsanpr_cobol_read_file(image_path, ..., result, result_len)`: Process image file
- `tsanpr_cobol_cleanup()`: Release resources

The wrapper handles:
- Dynamic loading of the TS-ANPR engine library
- Platform detection (Windows/Linux, x86/ARM)
- COBOL string conversion (fixed-length to null-terminated)
- Error handling and result formatting

### COBOL Program (`src/cobol/anpr.cbl`)

The COBOL program demonstrates:
- Single license plate recognition
- Multiple license plate recognition (vm)
- Multiple plates with motorcycles (vmb)
- Surround detection (vms)
- Object detection (dms, dmsr)
- Region of Interest (ROI) detection (dmsri)

All features match the functionality of examples in other languages (C, Python, etc.).

## Performance Notes

- **Windows**: Engine initialization takes approximately 1 second
- **WSL/Linux on Windows filesystem (`/mnt/`)**: Engine initialization may take 5-7 seconds due to cross-filesystem performance overhead
- **Native Linux**: Engine initialization takes approximately 1-2 seconds

For better performance on WSL, copy the entire project to the WSL native filesystem (e.g., `~/`).

## Troubleshooting

### Linux: "libcob: error: module not found"

This error occurs when GnuCOBOL cannot find the C wrapper library. The run script uses `LD_PRELOAD` to force load the library.

**Solution**: Always use `run.sh` or `make run` instead of running the binary directly.

### Windows: DLL not found error

Ensure that:
1. `bin/tsanpr_cobol.dll` exists after compilation
2. You're running via `run.bat` which sets `COB_LIBRARY_PATH`

## License

This example code is released under the MIT License.