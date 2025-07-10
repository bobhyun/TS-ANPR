English | [한국어](doc.i18n/README_ko-KR.md) | [日本語](doc.i18n/README_ja-JP.md) | [Tiếng Việt](doc.i18n/README_vi-VN.md)

# Python example

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
  └── Python
      └── anpr                   # source directory
         ├── anpr.py
         ├── webcam.py
         └── tsanpr
            ├── __init__.py
            └── tsanpr.py
  ```

### 2. How to Run

1. Install dependencies

   ```sh
   pip install opencv-python

   ```

2. Run `anpr`

   ```sh
   python anpr.py
   ```

3. Run `webcam`

   ```sh
   python webcam.py
   ```
