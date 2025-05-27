English | [日本語](doc.i18n/README_ja-JP.md) | [한국어](doc.i18n/README_ko-KR.md) | [Tiếng Việt](doc.i18n/README_vi-VN.md)

# Ruby example

### 1. Copying the Engine Files

_**[Note]** In this example, the engine file is extracted to the examples/bin/ directory to share it with other examples. However, for actual deployment, the engine file is typically copied to the directory where the application's executable file is located._

- For Windows x86 64-bit
  Extract the engine file to the `examples/bin/linux-x86_64` directory

  ```sh
  tar xvf tsanpr-linux-x86_64.tar.xz
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
  └── Ruby
      └── anpr                   # source directory
         ├── anpr.rb
         └── lib
            └── tsanpr.rb
  ```

### 2. Dependency Installation and Execution

1. Dependency Installation

   1. Windows

      ```sh
      # It is recommended to use Chocolatey.
      choco install vips
      gem install ffi ruby-vips
      ```

   2. Linux

      ```sh
      # Debian / Ubuntu Linux
      sudo apt-get install -y libvips

      # Oracle Linux / RedHat (RHEL) / CentOS
      sudo yum install vips

      sudo gem install ffi ruby-vips
      ```

2. How to Run

   ```sh
   ruby anpr.rb
   ```
