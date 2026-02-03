English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# Groovy Example

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Groovy/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Groovy/anpr)

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
  └── Groovy/
      └── anpr/                 # Groovy source directory
          ├── anpr.groovy       # main example
          └── TSANPR.groovy     # TSANPR JNA wrapper module
  ```

### 2. Prerequisites

1. Install Java JDK 8 or higher

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get update
   sudo apt-get install -y openjdk-11-jdk
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   sudo dnf install java-11-openjdk-devel
   ```

   **macOS:**

   ```sh
   brew install openjdk@11
   ```

   **Windows:**

   - Download and install JDK from https://adoptium.net/ or https://www.oracle.com/java/technologies/downloads/

2. Install Groovy

   **Ubuntu/Debian:**

   ```sh
   sudo apt-get install -y groovy
   ```

   **CentOS/RHEL/Fedora:**

   ```sh
   sudo dnf install groovy
   ```

   **macOS:**

   ```sh
   brew install groovy
   ```

   **Windows:**

   - Download from https://groovy.apache.org/download.html
   - Or use SDKMAN: `sdk install groovy`

3. Verify installation

   ```sh
   java -version
   groovy --version
   ```

### 3. How to Run

```sh
cd examples/Groovy/anpr

# Run the ANPR example
groovy anpr.groovy
```

### 4. Features

- **readImageFile**: Process image files directly using `anpr_read_file()`
- **readEncodedImage**: Process encoded image bytes using `anpr_read_pixels()` with 'encoded' format
- **readPixelBuffer**: Process raw RGB pixel data using Java AWT BufferedImage
- **Dynamic loading**: TSANPR library is loaded at runtime via JNA
- **Cross-platform**: Supports Windows and Linux (x86_64, x86, ARM64)
- **Automatic dependency**: JNA is automatically downloaded via Grape (@Grab annotation)

### 5. API Reference

#### TSANPR Module

The `TSANPR` class provides the following methods:

```groovy
// Load the TSANPR library
def tsanpr = new TSANPR(libraryPath)

// Initialize the ANPR engine
String error = tsanpr.initialize(mode)

// Read and process an image file
String result = tsanpr.readFile(imgFileName, outputFormat, options)

// Process pixel data directly
String result = tsanpr.readPixels(pixels, width, height, stride, pixelFormat, outputFormat, options)
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

This example uses JNA (Java Native Access) for dynamic library loading:

**Dynamic Library Loading:**
- JNA provides easy access to native shared libraries without JNI boilerplate
- The `TSANPRLibrary` interface defines native function signatures
- Library is loaded using `Native.load()` method

**Groovy Features:**
- Uses Grape (@Grab annotation) for automatic dependency management
- JNA library is downloaded automatically on first run
- Closures and method references for flexible function selection

**Pixel Buffer Processing:**
The `readPixelBuffer` function uses Java's `BufferedImage` to load and decode images. It extracts raw pixel data and passes it to `anpr_read_pixels()` with the appropriate pixel format using JNA's `Memory` class.

### 7. Troubleshooting

**Groovy not found:**
```sh
# Check if Groovy is installed
groovy --version

# Ubuntu/Debian: Install Groovy
sudo apt-get install groovy

# Or use SDKMAN
curl -s "https://get.sdkman.io" | bash
sdk install groovy
```

**JNA dependency issues:**
```sh
# Grape should automatically download JNA
# If issues persist, check network connectivity or proxy settings

# Clear Grape cache and retry
rm -rf ~/.groovy/grapes/net.java.dev.jna
groovy anpr.groovy
```

**Library loading issues:**
- Verify the TSANPR library exists in the expected location
- On Linux, check `LD_LIBRARY_PATH` if needed
- Ensure library architecture matches JVM (64-bit vs 32-bit)

**Runtime issues:**
- Check that engine files (`.eon`) are in the same directory as the library
- Verify license is installed using `tshelper`
