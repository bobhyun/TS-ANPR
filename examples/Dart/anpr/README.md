English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# Dart Example

https://github.com/bobhyun/TS-ANPR/tree/main/examples/Dart/anpr

### 1. Copying the Engine Files

_**[Note]** In this example, the engine file is extracted to the examples/bin/ directory to share it with other examples. However, for actual deployment, the engine file is typically copied to the directory where the application's executable file is located._

- For Windows x86 64-bit
  Extract the engine file to the `examples/bin/linux-x86_64` directory
  ```sh
  tar xvf tsanpr*-linux-x86_64.tar.xz
  ```
- For Windows x86 32-bit
  Extract the engine file to the `examples/bin/linux-x86` directory
  ```sh
  tar xvf tsanpr*-linux-x86.tar.xz
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
  ├── img                       # image directory
  └── Dart
      └── anpr
          ├── pubspec.yaml
          ├── bin               # source directory
          │   ├── anpr.dart
          │   └── anpr.exe      # compiled executable
          └── lib               # library directory
              └── tsanpr.dart
  ```

### 2. Build and Run

```sh
# Install dependencies
dart pub get

# Run directly from source code
dart run bin/anpr.dart

# Compile to create an executable file
dart compile exe bin/anpr.dart

# Run the compiled executable
bin/anpr
```
