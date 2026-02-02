English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# R Example

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/R/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/R/anpr)

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
  └── R
      └── anpr                   # source directory
         ├── anpr.R              # main example script
         ├── tsanpr.R            # TSANPR R6 wrapper class
         ├── src/                # C wrapper source
         │   ├── tsanpr_r.c
         │   ├── Makevars
         │   └── Makevars.win
         └── DESCRIPTION
  ```

### 2. Prerequisites

1. Install R and system dependencies

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get update
   # Install R and development tools
   sudo apt-get install -y r-base r-base-dev
   # Install system libraries for R packages
   sudo apt-get install -y libcurl4-openssl-dev libmagick++-dev
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   # CentOS/RHEL 8+ / Fedora
   sudo dnf install -y R R-devel
   sudo dnf install -y libcurl-devel ImageMagick-c++-devel

   # CentOS/RHEL 7
   sudo yum install -y R R-devel
   sudo yum install -y libcurl-devel ImageMagick-c++-devel
   ```

   **Windows:**

   - Download R from https://cran.r-project.org/bin/windows/base/
   - Install Rtools from https://cran.r-project.org/bin/windows/Rtools/

2. Install required R packages

   ```sh
   # Install R6 package (required)
   sudo Rscript -e 'install.packages("R6", repos="https://cloud.r-project.org")'

   # Install magick package (optional, for readPixelBuffer)
   sudo Rscript -e 'install.packages("magick", repos="https://cloud.r-project.org")'
   ```

   _Note: On Windows, run the Rscript commands without `sudo`._

3. Compile the C wrapper

   **Linux/macOS:**

   ```sh
   cd examples/R/anpr
   R CMD SHLIB src/tsanpr_r.c -o src/tsanpr_r.so
   ```

   **Windows:**

   ```sh
   cd examples/R/anpr
   R CMD SHLIB src/tsanpr_r.c -o src/tsanpr_r.dll
   ```

### 3. How to Run

```sh
cd examples/R/anpr

# Run the ANPR example
Rscript anpr.R
```

**From R console:**

```r
setwd("examples/R/anpr")
source("anpr.R")
```

**Interactive mode:**

```r
# Load the TSANPR wrapper
source("tsanpr.R")

# Initialize TSANPR
engine_path <- "../../bin/linux-x86_64/libtsanpr.so"  # Adjust for your platform
tsanpr <- TSANPR$new(engine_path)

# Initialize engine
tsanpr$anpr_initialize("text;country=KR")

# Process an image
result <- tsanpr$anpr_read_file("../../img/KR/licensePlate.jpg", "text", "")
print(result)
```

### 4. Features

- **readImageFile**: Process image files directly using `anpr_read_file()`
- **readEncodedImage**: Process encoded image bytes using `anpr_read_pixels()` with 'encoded' format
- **readPixelBuffer**: Process raw RGB pixel data using magick package
- **Dynamic loading**: TSANPR library is loaded at runtime via C wrapper
- **Cross-platform**: Supports Windows and Linux (x86_64, ARM64)

### 5. API Reference

#### TSANPR Class

The `TSANPR` R6 class provides the following methods:

```r
TSANPR <- R6Class("TSANPR",
  public = list(
    initialize = function(library_path),
    anpr_initialize = function(mode),
    anpr_read_file = function(img_file_name, output_format, options),
    anpr_read_pixels = function(pixels, width, height, stride, pixel_format, output_format, options),
    is_loaded = function()
  )
)
```

#### Recognition Options

| Option | Description |
|--------|-------------|
| `""` | Single license plate recognition (default) |
| `"vm"` | Recognize multiple license plates attached to vehicles |
| `"vmb"` | Recognize multiple license plates (including motorcycles) |
| `"vms"` | Recognize with surround detection |
| `"dms"` | Detect multiple surrounding objects (vehicles) |
| `"dmsr"` | Detect objects and recognize license plates |
| `"dmsri<coords>"` | Recognize within Region of Interest |

#### Output Formats

`"text"`, `"json"`, `"yaml"`, `"xml"`, `"csv"`

### 6. Implementation Notes

This example uses a C wrapper (`src/tsanpr_r.c`) to bridge R's FFI with the TSANPR library:

**Dynamic Library Loading:**
- **Linux**: Uses `dlopen()`, `dlsym()`, `dlclose()` from libdl
- **Windows**: Uses `LoadLibrary()`, `GetProcAddress()`, `FreeLibrary()` from kernel32

**R Integration:**
- Uses R's `.Call()` interface for efficient data exchange
- Properly registers C routines via `R_registerRoutines()`
- R6 class provides clean object-oriented interface

The R code loads the compiled C wrapper, which in turn dynamically loads and calls the TSANPR library functions.

### 7. Troubleshooting

**Compilation Issues:**

- Ensure R development packages are installed (`r-base-dev` on Debian/Ubuntu)
- On Windows, install Rtools and add it to PATH
- Check that the C compiler is available: `R CMD config CC`

**Library Loading Issues:**

- Verify the compiled wrapper exists in `src/` directory
- Ensure the TSANPR library path is correct
- On Linux, set `LD_LIBRARY_PATH` if needed

**Package Dependencies:**

- Install R6 package: `install.packages("R6")`
- For pixel buffer processing: `install.packages("magick")`
