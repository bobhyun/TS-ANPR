// The MIT License (MIT)
// Copyright © 2022-2025 TS-Solution Corp.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to all conditions.

// Link C wrapper library
use "lib:ffi_wrapper"

// Link platform-specific libraries for dynamic loading
use "lib:dl" if linux
use "lib:dl" if osx
use "lib:m"

// TSANPR C wrapper function declarations
use @tsanpr_load[I32](library_path: Pointer[U8] tag)
use @tsanpr_unload[None]()
use @tsanpr_is_loaded[I32]()
use @tsanpr_initialize[Pointer[U8]](mode: Pointer[U8] tag)
use @tsanpr_read_file[Pointer[U8]](img_file: Pointer[U8] tag, output_format: Pointer[U8] tag, options: Pointer[U8] tag)
use @tsanpr_read_pixels[Pointer[U8]](pixels: Pointer[U8] tag, width: U64, height: U64, stride: I64, pixel_format: Pointer[U8] tag, output_format: Pointer[U8] tag, options: Pointer[U8] tag)

// Image loader convenience function (loads image via stb_image and calls ANPR)
use @image_anpr_read[Pointer[U8]](filename: Pointer[U8] tag, output_format: Pointer[U8] tag, options: Pointer[U8] tag)

class TSANPR
  var _loaded: Bool = false

  new create(library_path: String) ? =>
    let result = @tsanpr_load(library_path.cstring())
    if result != 0 then
      error
    end
    _loaded = true

  fun ref initialize(mode: String): String =>
    let result = @tsanpr_initialize(mode.cstring())
    if result.is_null() then
      "Error: Failed to initialize ANPR engine"
    else
      String.from_cstring(result).clone()
    end

  fun ref read_file(img_file_name: String, output_format: String, options: String): String =>
    let result = @tsanpr_read_file(img_file_name.cstring(), output_format.cstring(), options.cstring())
    if result.is_null() then
      "Error: Failed to read image file"
    else
      String.from_cstring(result).clone()
    end

  fun ref read_pixels(pixels: Array[U8] val, width: U64, height: U64, stride: I64,
                     pixel_format: String, output_format: String, options: String): String =>
    let result = @tsanpr_read_pixels(pixels.cpointer(), width, height, stride,
                                    pixel_format.cstring(), output_format.cstring(), options.cstring())
    if result.is_null() then
      "Error: Failed to read pixel data"
    else
      String.from_cstring(result).clone()
    end

  fun _final() =>
    if _loaded then
      @tsanpr_unload()
    end
