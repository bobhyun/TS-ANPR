// The MIT License (MIT)
// Copyright © 2022-2025 TS-Solution Corp.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to all conditions.

package main

import "core:c"
import "core:fmt"
import "core:os"
import "core:path/filepath"
import "core:strings"
import "stb_image"
import "tsanpr"

// Get the examples base directory at runtime
get_examples_base_dir :: proc() -> string {
    cwd := os.get_current_directory()
    return filepath.join({filepath.dir(filepath.dir(cwd))})
}

// Generate engine filename depending on platform and architecture
get_engine_file_name :: proc() -> string {
    base_dir := get_examples_base_dir()
    when ODIN_OS == .Windows {
        when ODIN_ARCH == .amd64 {
            return filepath.join({base_dir, "bin", "windows-x86_64", "tsanpr.dll"})
        } else when ODIN_ARCH == .i386 {
            return filepath.join({base_dir, "bin", "windows-x86", "tsanpr.dll"})
        }
    } else when ODIN_OS == .Linux {
        when ODIN_ARCH == .amd64 {
            return filepath.join({base_dir, "bin", "linux-x86_64", "libtsanpr.so"})
        } else when ODIN_ARCH == .arm64 {
            return filepath.join({base_dir, "bin", "linux-aarch64", "libtsanpr.so"})
        }
    }
    return ""
}

// Read image file using anpr_read_file
read_image_file :: proc(tsanpr_inst: ^tsanpr.TSANPR, imgfile: string, output_format: string, options: string) {
    fmt.printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, output_format, options)
    result := tsanpr.read_file(tsanpr_inst, imgfile, output_format, options)
    fmt.println(result)
}

// Read encoded image file as bytes
read_encoded_image :: proc(tsanpr_inst: ^tsanpr.TSANPR, imgfile: string, output_format: string, options: string) {
    fmt.printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, output_format, options)

    data, ok := os.read_entire_file(imgfile)
    if !ok {
        fmt.println("File does not exist")
        return
    }
    defer delete(data)

    result := tsanpr.read_pixels(tsanpr_inst, data, u64(len(data)), 0, 0, "encoded", output_format, options)
    fmt.println(result)
}

// Read pixel buffer using stb_image for decoding
read_pixel_buffer :: proc(tsanpr_inst: ^tsanpr.TSANPR, imgfile: string, output_format: string, options: string) {
    fmt.printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, output_format, options)

    imgfile_cstr := strings.clone_to_cstring(imgfile)
    defer delete(imgfile_cstr)

    width, height, channels: c.int
    // Load image with 3 channels (RGB)
    pixels := stb_image.load(imgfile_cstr, &width, &height, &channels, 3)
    if pixels == nil {
        fmt.println("Failed to load image")
        return
    }
    defer stb_image.image_free(pixels)

    stride := i64(width) * 3  // 3 bytes per pixel (RGB)
    pixel_data := ([^]u8)(pixels)[:width * height * 3]

    result := tsanpr.read_pixels(tsanpr_inst, pixel_data, u64(width), u64(height), stride, "RGB", output_format, options)
    fmt.println(result)
}

// Read license plates with specified country code
read_license_plates :: proc(tsanpr_inst: ^tsanpr.TSANPR, country_code: string) {
    // NOTICE:
    // anpr_initialize should be called only once after library load.
    // Therefore, it is not possible to change the country code after anpr_initialize has been called.
    // While using the free trial license, you can try all languages.
    // When you purchase a commercial license, you can only use the selected language.

    init_params := strings.concatenate({"text;country=", country_code})
    defer delete(init_params)

    error := tsanpr.initialize(tsanpr_inst, init_params)
    if len(error) > 0 {
        fmt.printf("anpr_initialize() failed: %s\n", error)
        return
    }

    base_dir := get_examples_base_dir()
    image_dir := filepath.join({base_dir, "img", country_code})

    // TODO: Try each function as needed
    anpr_func := read_image_file
    // anpr_func := read_encoded_image
    // anpr_func := read_pixel_buffer

    // TODO: Try each output format as needed
    output_format := "text"
    // output_format := "json"
    // output_format := "yaml"
    // output_format := "xml"
    // output_format := "csv"

    anpr_func(tsanpr_inst, filepath.join({image_dir, "licensePlate.jpg"}), output_format, "")      // Single license plate recognition (default)
    anpr_func(tsanpr_inst, filepath.join({image_dir, "multiple.jpg"}), output_format, "vm")        // Recognize multiple license plates attached to vehicles
    anpr_func(tsanpr_inst, filepath.join({image_dir, "multiple.jpg"}), output_format, "vmb")       // Recognize multiple license plates attached to vehicles (including motorcycles)
    anpr_func(tsanpr_inst, filepath.join({image_dir, "surround.jpg"}), output_format, "vms")       // Recognize multiple license plates attached to vehicles with surround detection
    anpr_func(tsanpr_inst, filepath.join({image_dir, "surround.jpg"}), output_format, "dms")       // Recognize multiple surrounding objects (vehicles)
    anpr_func(tsanpr_inst, filepath.join({image_dir, "surround.jpg"}), output_format, "dmsr")      // Recognize multiple surrounding objects (vehicles) and license plates

    // Recognize multiple surrounding objects and license plates within RoI
    anpr_func(tsanpr_inst, filepath.join({image_dir, "surround.jpg"}), output_format, "dmsri549,700,549,2427,1289,2427,1289,700")
}

main :: proc() {
    engine_file := get_engine_file_name()
    if engine_file == "" || !os.exists(engine_file) {
        fmt.println("Unsupported operating system or engine not found")
        return
    }

    tsanpr_inst, ok := tsanpr.create(engine_file)
    if !ok {
        fmt.println("TSANPR initialization failed")
        return
    }
    defer tsanpr.destroy(&tsanpr_inst)

    // TODO: Try each country code as needed
    read_license_plates(&tsanpr_inst, "KR")
    // read_license_plates(&tsanpr_inst, "JP")
    // read_license_plates(&tsanpr_inst, "VN")
}
