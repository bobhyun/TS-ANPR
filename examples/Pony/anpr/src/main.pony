// The MIT License (MIT)
// Copyright © 2022-2025 TS-Solution Corp.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to all conditions.

use "files"

actor Main
  let _env: Env
  let _examples_base_dir: String

  new create(env: Env) =>
    _env = env
    _examples_base_dir = "../.."

    let engine_file = get_engine_file_name()
    if engine_file == "" then
      _env.out.print("Unsupported platform")
      return
    end

    try
      let tsanpr = TSANPR(engine_file)?

      // TODO: Try each country code as needed
      read_license_plates(tsanpr, "KR")
      // read_license_plates(tsanpr, "JP")
      // read_license_plates(tsanpr, "VN")

    else
      _env.out.print("TSANPR initialization failed")
      _env.out.print("Please extract the TSANPR engine files to the bin directory.")
    end

  fun get_engine_file_name(): String =>
    ifdef windows then
      ifdef ilp32 then
        _examples_base_dir + "/bin/windows-x86/tsanpr.dll"
      else
        _examples_base_dir + "/bin/windows-x86_64/tsanpr.dll"
      end
    else
      ifdef arm then
        _examples_base_dir + "/bin/linux-aarch64/libtsanpr.so"
      else
        _examples_base_dir + "/bin/linux-x86_64/libtsanpr.so"
      end
    end

  fun read_license_plates(tsanpr: TSANPR, country_code: String) =>
    """
    NOTICE:
    anpr_initialize should be called only once after library load.
    Therefore, it is not possible to change the country code after anpr_initialize has been called.
    While using the free trial license, you can try all languages.
    When you purchase a commercial license, you can only use the selected language.
    """
    let init_error = tsanpr.initialize("text;country=" + country_code)
    if init_error != "" then
      _env.out.print("anpr_initialize() failed: " + init_error)
      return
    end

    let image_dir = _examples_base_dir + "/img/" + country_code

    // TODO: Try each function as needed
    let anpr_func = "readImageFile"
    // let anpr_func = "readEncodedImage"
    // let anpr_func = "readPixelBuffer"

    // TODO: Try each output format as needed
    let output_format = "text"
    // let output_format = "json"
    // let output_format = "yaml"
    // let output_format = "xml"
    // let output_format = "csv"

    anpr_read(tsanpr, anpr_func, image_dir + "/licensePlate.jpg", output_format, "")        // Single license plate recognition (default)
    anpr_read(tsanpr, anpr_func, image_dir + "/multiple.jpg", output_format, "vm")          // Recognize multiple license plates attached to vehicles
    anpr_read(tsanpr, anpr_func, image_dir + "/multiple.jpg", output_format, "vmb")         // Recognize multiple license plates attached to vehicles (including motorcycles)
    anpr_read(tsanpr, anpr_func, image_dir + "/surround.jpg", output_format, "vms")         // Recognize multiple license plates attached to vehicles with surround detection
    anpr_read(tsanpr, anpr_func, image_dir + "/surround.jpg", output_format, "dms")         // Recognize multiple surrounding objects (vehicles)
    anpr_read(tsanpr, anpr_func, image_dir + "/surround.jpg", output_format, "dmsr")        // Recognize multiple surrounding objects (vehicles) and license plates
    anpr_read(tsanpr, anpr_func, image_dir + "/surround.jpg", output_format, "dmsri549,700,549,2427,1289,2427,1289,700")  // Recognize within RoI

  fun anpr_read(tsanpr: TSANPR, func: String, imgfile: String, output_format: String, options: String) =>
    match func
    | "readImageFile" => read_image_file(tsanpr, imgfile, output_format, options)
    | "readEncodedImage" => read_encoded_image(tsanpr, imgfile, output_format, options)
    | "readPixelBuffer" => read_pixel_buffer(tsanpr, imgfile, output_format, options)
    end

  fun read_image_file(tsanpr: TSANPR, imgfile: String, output_format: String, options: String) =>
    """
    Read an image file and call anpr_read_file.
    """
    _env.out.print(imgfile + " (outputFormat=\"" + output_format + "\", options=\"" + options + "\") => ")
    let result = tsanpr.read_file(imgfile, output_format, options)
    _env.out.print(result)

  fun read_encoded_image(tsanpr: TSANPR, imgfile: String, output_format: String, options: String) =>
    """
    Read an encoded image file as bytes and call anpr_read_pixels with 'encoded' pixel format.
    """
    _env.out.print(imgfile + " (outputFormat=\"" + output_format + "\", options=\"" + options + "\") => ")

    let path = FilePath(FileAuth(_env.root), imgfile)
    match OpenFile(path)
    | let file: File =>
      let data: Array[U8] iso = file.read(file.size())
      let pixels: Array[U8] val = consume data

      let result = tsanpr.read_pixels(pixels, pixels.size().u64(), 0, 0, "encoded", output_format, options)
      _env.out.print(result)
    else
      _env.out.print("Error: Cannot open file")
    end

  fun read_pixel_buffer(tsanpr: TSANPR, imgfile: String, output_format: String, options: String) =>
    """
    Read raw pixel buffer using stb_image library.
    Uses C convenience function that loads image and calls ANPR directly.
    """
    _env.out.print(imgfile + " (outputFormat=\"" + output_format + "\", options=\"" + options + "\") => ")

    // Use C convenience function that handles image loading and ANPR processing
    let result = @image_anpr_read(imgfile.cstring(), output_format.cstring(), options.cstring())

    if result.is_null() then
      _env.out.print("Error: Failed to process image")
    else
      _env.out.print(String.from_cstring(result).clone())
    end
