English | [日本語](doc.i18n/README_ja-JP.md) | [한국어](doc.i18n/README_ko-KR.md) | [Tiếng Việt](doc.i18n/README_vi-VN.md)

# Go example

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
  │   ├─── windows-x86_64        # engine directory for windows (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   ├── linux-x86_64           # engine directory for linux (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # engine directory for linux (arm64)
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # image directory
  └── Go
      └── anpr                   # source directory
         ├── anpr                # compiled executable for Windows
         ├── anpr.exe            # compiled executable for Linux
         ├── go.mod
         ├── main.go
         └── tsanpr
            ├── tsanpr_windows.go
            └── tsanpr_linux.go
  ```

### 2. Build and Run

#### 2.1 Windows (MinGW)

1. Module Initialzation

   ```sh
   go mod tidy
   ```

2. Dependency Installation

   - Refer to the official GoCV installation guide. (https://gocv.io/getting-started/windows/)

3. How to Build

   ```sh
   go build -o anpr.exe main.go
   ```

4. How to Run

   ```sh
   .\anpr.exe
   ```

#### 2.2. Linux

1. Module Initialzation

   ```sh
   go mod tidy
   ```

2. Dependency Installation

   - Debian / Ubuntu Linux

     ```sh
     sudo apt install -y build-essential g++ libopencv-dev
     ```

   - Oracle Linux / RedHat (RHEL) / CentOS

     ```sh
     sudo def install -y epel-release gcc gcc-c++ opencv opencv-devel
     ```

3. How to Build

   ```sh
   go build -o anpr main.go
   ```

4. How to Run

   ```sh
   ./anpr
   ```
