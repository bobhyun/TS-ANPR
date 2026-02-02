// The MIT License (MIT)
// Copyright © 2022-2025 TS-Solution Corp.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to all conditions.

package tsanpr

import "core:c"
import "core:dynlib"
import "core:fmt"
import "core:strings"

// Function pointer types
Anpr_Initialize_Proc :: #type proc "c" (mode: cstring) -> cstring
Anpr_Read_File_Proc :: #type proc "c" (img_file_name: cstring, output_format: cstring, options: cstring) -> cstring
Anpr_Read_Pixels_Proc :: #type proc "c" (pixels: rawptr, width: c.uint64_t, height: c.uint64_t, stride: c.int64_t, pixel_format: cstring, output_format: cstring, options: cstring) -> cstring

// TSANPR handle type
TSANPR :: struct {
    lib: dynlib.Library,
    anpr_initialize: Anpr_Initialize_Proc,
    anpr_read_file: Anpr_Read_File_Proc,
    anpr_read_pixels: Anpr_Read_Pixels_Proc,
}

// Create new TSANPR instance
create :: proc(library_path: string) -> (TSANPR, bool) {
    lib, lib_ok := dynlib.load_library(library_path)
    if !lib_ok {
        fmt.printf("Failed to load library: %s\n", library_path)
        return {}, false
    }

    inst := TSANPR{
        lib = lib,
    }

    // Load function pointers
    ptr1, ok1 := dynlib.symbol_address(lib, "anpr_initialize")
    if !ok1 {
        fmt.println("Failed to load anpr_initialize")
        dynlib.unload_library(lib)
        return {}, false
    }
    inst.anpr_initialize = cast(Anpr_Initialize_Proc)ptr1

    ptr2, ok2 := dynlib.symbol_address(lib, "anpr_read_file")
    if !ok2 {
        fmt.println("Failed to load anpr_read_file")
        dynlib.unload_library(lib)
        return {}, false
    }
    inst.anpr_read_file = cast(Anpr_Read_File_Proc)ptr2

    ptr3, ok3 := dynlib.symbol_address(lib, "anpr_read_pixels")
    if !ok3 {
        fmt.println("Failed to load anpr_read_pixels")
        dynlib.unload_library(lib)
        return {}, false
    }
    inst.anpr_read_pixels = cast(Anpr_Read_Pixels_Proc)ptr3

    return inst, true
}

// Destroy TSANPR instance
destroy :: proc(inst: ^TSANPR) {
    if inst.lib != nil {
        dynlib.unload_library(inst.lib)
        inst.lib = nil
    }
}

// Initialize ANPR engine
initialize :: proc(inst: ^TSANPR, mode: string) -> string {
    mode_cstr := strings.clone_to_cstring(mode)
    defer delete(mode_cstr)

    result := inst.anpr_initialize(mode_cstr)
    return strings.clone_from_cstring(result)
}

// Read image file
read_file :: proc(inst: ^TSANPR, img_file_name: string, output_format: string, options: string) -> string {
    img_cstr := strings.clone_to_cstring(img_file_name)
    defer delete(img_cstr)

    format_cstr := strings.clone_to_cstring(output_format)
    defer delete(format_cstr)

    options_cstr := strings.clone_to_cstring(options)
    defer delete(options_cstr)

    result := inst.anpr_read_file(img_cstr, format_cstr, options_cstr)
    return strings.clone_from_cstring(result)
}

// Read pixel data
read_pixels :: proc(inst: ^TSANPR, pixels: []u8, width: u64, height: u64, stride: i64, pixel_format: string, output_format: string, options: string) -> string {
    pixel_format_cstr := strings.clone_to_cstring(pixel_format)
    defer delete(pixel_format_cstr)

    format_cstr := strings.clone_to_cstring(output_format)
    defer delete(format_cstr)

    options_cstr := strings.clone_to_cstring(options)
    defer delete(options_cstr)

    result := inst.anpr_read_pixels(raw_data(pixels), width, height, stride, pixel_format_cstr, format_cstr, options_cstr)
    return strings.clone_from_cstring(result)
}
