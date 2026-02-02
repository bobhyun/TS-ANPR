English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# Pony Example

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Pony/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Pony/anpr)

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
  └── Pony
      └── anpr                   # project directory
         ├── src/                # main package
         │   ├── main.pony
         │   └── tsanpr.pony
         ├── ffi/                # C FFI wrapper
         │   ├── tsanpr_wrapper.c
         │   ├── tsanpr_wrapper.h
         │   ├── image_loader.c
         │   └── stb_image.h
         └── Makefile
  ```

### 2. Prerequisites

1. Install Pony compiler

   **Ubuntu/Debian:**

   ```sh
   # Using ponyup (recommended)
   curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/ponylang/ponyup/latest-release/ponyup-init.sh | sh
   source ~/.profile
   ponyup update ponyc release
   ```

   **macOS:**

   ```sh
   brew install ponyc
   ```

   **Windows:**

   ```sh
   # Using Chocolatey
   choco install ponyc

   # Or download from https://github.com/ponylang/ponyc/releases
   ```

2. Install C compiler (GCC or Clang)

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get install build-essential
   ```

   **macOS:**

   ```sh
   xcode-select --install
   ```

   **Windows:**

   - Install Visual Studio Build Tools or MinGW-w64

3. Verify installation

   ```sh
   ponyc --version
   gcc --version
   ```

### 3. How to Run

```sh
cd examples/Pony/anpr

# Build and run
make run

# Build only
make

# Build release version
make release

# Clean build artifacts
make clean
```

### 4. Features

- **readImageFile**: Process image files directly using `anpr_read_file()`
- **readEncodedImage**: Process encoded image bytes using `anpr_read_pixels()` with 'encoded' format
- **readPixelBuffer**: Process raw RGB pixel data using stb_image library
- **Dynamic loading**: TSANPR library is loaded at runtime using dlopen/LoadLibrary
- **Cross-platform**: Supports Windows and Linux (x86_64, ARM64)

### 5. API Reference

#### TSANPR Class

```pony
class TSANPR
  new create(library_path: String) ?
  fun ref initialize(mode: String): String
  fun ref read_file(img_file_name: String, output_format: String, options: String): String
  fun ref read_pixels(pixels: Array[U8] val, width: U64, height: U64, stride: I64,
                     pixel_format: String, output_format: String, options: String): String
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

This example uses C wrappers to handle dynamic library loading and image processing:

**Dynamic Library Loading (`tsanpr_wrapper.c`):**
- **Linux**: Uses `dlopen()`, `dlsym()`, `dlclose()` from libdl
- **Windows**: Uses `LoadLibrary()`, `GetProcAddress()`, `FreeLibrary()` from kernel32

**Image Processing (`image_loader.c`):**
- Uses [stb_image](https://github.com/nothings/stb) single-header library for image decoding
- Provides convenience function `image_anpr_read()` that loads image and calls ANPR directly

The Pony code calls the C wrapper functions via FFI, which in turn call the dynamically loaded TSANPR library functions.
