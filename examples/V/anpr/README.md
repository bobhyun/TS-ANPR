English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# V Example

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/V/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/V/anpr)

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
  ```
  examples/
  ├── bin/
  │   ├── windows-x86_64/       # engine for Windows (x86_64)
  │   │   ├── tsanpr.dll
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper.exe
  │   ├── windows-x86/          # engine for Windows (x86)
  │   │   └── ...
  │   ├── linux-x86_64/         # engine for Linux (x86_64)
  │   │   └── ...
  │   └── linux-aarch64/        # engine for Linux (arm64)
  │       └── ...
  ├── img/
  └── V/
      └── anpr/                 # V module directory
          ├── v.mod             # module definition
          ├── anpr.v            # main example
          └── tsanpr.v          # TSANPR wrapper module
  ```

### 2. Prerequisites

1. Install V (latest version recommended)

   **Windows:**

   ```sh
   # Using Scoop
   scoop install vlang

   # Or download pre-built binary from:
   # https://github.com/vlang/v/releases
   ```

   **Linux:**

   ```sh
   # Clone and build from source
   git clone https://github.com/vlang/v
   cd v
   make
   sudo ./v symlink
   ```

2. Verify installation

   ```sh
   v version
   ```

### 3. How to Run

```sh
cd examples/V/anpr

# Run the ANPR example
v run .
```

**Alternative methods:**

```sh
# Compile and run separately
v .
./anpr        # Linux
anpr.exe      # Windows

# Compile with optimizations
v -prod .
```

### 4. Features

- **readImageFile**: Process image files directly using `anpr_read_file()`
- **readEncodedImage**: Process encoded image bytes using `anpr_read_pixels()` with 'encoded' format
- **readPixelBuffer**: Process raw RGB pixel data using V's `stbi` module (stb_image)
- **Dynamic loading**: TSANPR library is loaded at runtime via V's `dl` module
- **Cross-platform**: Supports Windows and Linux (x86_64, x86, ARM64)

### 5. API Reference

#### TSANPR Module

The `tsanpr` module provides the following:

```v
// Create TSANPR instance
pub fn new(library_path string) !TSANPR

// TSANPR methods
pub fn (mut t TSANPR) destroy()
pub fn (t &TSANPR) initialize(mode string) string
pub fn (t &TSANPR) read_file(img_file_name string, output_format string, options string) string
pub fn (t &TSANPR) read_pixels(pixels []u8, width u64, height u64, stride i64, pixel_format string, output_format string, options string) string
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

This example uses V's `dl` module for dynamic library loading:

**Dynamic Library Loading:**
- **Linux**: Uses `dlopen()`, `dlsym()`, `dlclose()` via V's `dl` module
- **Windows**: Uses `LoadLibrary()`, `GetProcAddress()`, `FreeLibrary()` via V's `dl` module

**V Integration:**
- V's C interop allows direct function pointer calls to native libraries
- Automatic string conversion between V strings and C strings
- Memory-safe with `defer` for cleanup

**Pixel Buffer Processing:**
The `readPixelBuffer` function uses V's built-in `stbi` module (stb_image wrapper) for image decoding. It loads the image, extracts raw RGB pixel data, and passes it to `anpr_read_pixels()` with the appropriate pixel format.

### 7. Troubleshooting

**Compilation Issues:**

- Ensure V is properly installed and in PATH
- Check V version: `v version`
- Update V if needed: `v up`

**Library Loading Issues:**

- Verify the TSANPR library exists in the expected location
- On Linux, check `LD_LIBRARY_PATH` if needed
- Ensure library architecture matches the V build (64-bit vs 32-bit)

**Runtime Issues:**

- Check that engine files (`.eon`) are in the same directory as the library
- Verify license is installed using `tshelper`
