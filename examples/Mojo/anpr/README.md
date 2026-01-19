English | [한국어](doc.i18n/ko-KR/) | [日本語](doc.i18n/ja-JP/) | [Tiếng Việt](doc.i18n/vi-VN/)

# Mojo example

[https://github.com/bobhyun/TS-ANPR/tree/main/examples/Mojo/anpr](https://github.com/bobhyun/TS-ANPR/tree/main/examples/Mojo/anpr)

### 1. Copying the Engine Files

_**[Note]** Mojo currently only supports Linux. In this example, the engine file is extracted to the examples/bin/ directory to share it with other examples. However, for actual deployment, the engine file is typically copied to the directory where the application's executable file is located._

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
  │   ├── linux-x86_64           # engine directory for Linux (x86_64)
  │   │   ├── libtsanpr.so
  │   │   ├── tsanpr-2505M.eon
  │   │   └── tshelper
  │   └── linux-aarch64          # engine directory for Linux (arm64)
  │       ├── libtsanpr.so
  │       ├── tsanpr-2505M.eon
  │       └── tshelper
  ├── img                        # image directory
  └── Mojo
      └── anpr                   # project directory
          ├── pixi.toml
          └── src                # source directory
              ├── anpr.mojo
              ├── webcam.mojo
              └── tsanpr.mojo
  ```

### 2. How to Run

1. Install Mojo SDK

   - Install pixi (package manager)
     ```sh
     curl -fsSL https://pixi.sh/install.sh | sh
     source ~/.bashrc  # or ~/.zshrc
     ```

   - Install Mojo using pixi (already configured in pixi.toml)
     ```sh
     cd examples/Mojo/anpr
     pixi install
     ```

   For more details, see: https://docs.modular.com/mojo/manual/install/

2. Run `anpr`

   ```sh
   pixi run anpr
   ```

3. Run `webcam`

   ```sh
   pixi run webcam
   ```
