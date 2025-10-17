English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# Ada example

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Ada/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Ada/anpr)

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
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- For Linux arm 64-bit
  Extract the engine file to the `examples/bin/linux-aarch64` directory
  ```sh
  tar xvf tsanpr*-linux-aarch64.tar.xz
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
  └── Ada
      └── anpr                   # project directory
         ├── bin                 # executable directory
         ├── obj                 # object files directory
         ├── src                 # source directory
         │   ├── anpr.adb
         │   ├── tsanpr.ads
         │   ├── tsanpr-windows.adb
         │   └── tsanpr-unix.adb
         ├── anpr.gpr
         ├── alire.toml
         ├── compile.bat
         ├── compile.sh
         └── Makefile
  ```

### 2. Build and Run

#### 2.1 Using Alire (Recommended)

[Alire](https://alire.ada.dev/) is the modern package manager for Ada that automatically manages toolchains and dependencies.

1. Install Alire

   **Windows:**

   - Download from [https://alire.ada.dev](https://alire.ada.dev)
   - Extract and add `alr` to your PATH

   **Linux:**

   **Recommended: Manual installation (all distributions)**

   Download the latest version from [Alire Releases](https://github.com/alire-project/alire/releases):

   ```sh
   # Download the latest release (check the releases page for current version)
   wget https://github.com/alire-project/alire/releases/download/v2.1.0/alr-2.1.0-bin-x86_64-linux.zip
   unzip alr-2.1.0-bin-x86_64-linux.zip
   sudo mv bin/alr /usr/local/bin/
   ```

   Or visit [https://alire.ada.dev](https://alire.ada.dev) to download and add to PATH

   **Alternative: Using package manager (may have older version)**

   - Debian/Ubuntu:
     ```sh
     sudo apt-get update
     sudo apt-get install alire
     ```
   - Fedora/RHEL/CentOS:
     ```sh
     sudo dnf install alire
     ```

   **Note:** If you encounter `Unexpected property count: 0` error with package manager installed version, please use the manual installation method above to get the latest version.

2. Build

   ```sh
   alr build
   ```

   On first build, Alire will automatically download and install GNAT compiler and GPRbuild. Simply press Enter when prompted.

3. Run

   ```sh
   alr run
   ```

   Or run directly:

   ```sh
   bin/anpr
   ```

#### 2.2 Using GNAT/GPRbuild (Traditional Method)

##### 2.2.1 Windows

1. GNAT Installation

   - Download and install [GNAT Community](https://www.adacore.com/download)
   - Add GNAT bin directory to PATH

2. How to Build

   ```cmd
   compile.bat
   ```

   Or using GPRbuild:

   ```cmd
   gprbuild -p -P anpr.gpr -XOS=Windows_NT
   ```

3. How to Run

   For proper UTF-8 character display (recommended for non-ASCII characters):

   ```cmd
   chcp 65001
   bin\anpr.exe
   ```

   Or without UTF-8 encoding (may show corrupted non-ASCII text):

   ```cmd
   bin\anpr.exe
   ```

##### 2.2.2 Linux

1. Dependency Installation

   - Debian / Ubuntu Linux

     ```sh
     sudo apt-get update
     sudo apt-get install gnat gprbuild
     ```

   - Oracle Linux / RedHat (RHEL) / CentOS

     ```sh
     sudo yum install gcc-gnat gprbuild
     ```

   - Fedora

     ```sh
     sudo dnf install gcc-gnat gprbuild
     ```

2. How to Build

   ```sh
   chmod +x compile.sh
   ./compile.sh
   ```

   Or using GPRbuild:

   ```sh
   gprbuild -p -P anpr.gpr -XOS=UNIX
   ```

   Or using Make:

   ```sh
   make
   ```

3. How to Run

   ```sh
   bin/anpr
   ```

   **Note for Character Encoding:**
   Non-ASCII characters should display correctly on Linux systems that support UTF-8 by default. If you encounter display issues, ensure your terminal is set to UTF-8 encoding.
