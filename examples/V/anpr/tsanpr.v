// The MIT License (MIT)
// Copyright © 2022-2025 TS-Solution Corp.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to all conditions.

module tsanpr

import dl

// Function pointer types
type FnInitialize = fn (mode &char) &char
type FnReadFile = fn (img_file_name &char, output_format &char, options &char) &char
type FnReadPixels = fn (pixels voidptr, width u64, height u64, stride i64, pixel_format &char, output_format &char, options &char) &char

// TSANPR structure
pub struct TSANPR {
mut:
	lib_handle       voidptr       = unsafe { nil }
	anpr_initialize  FnInitialize  = unsafe { nil }
	anpr_read_file   FnReadFile    = unsafe { nil }
	anpr_read_pixels FnReadPixels  = unsafe { nil }
}

// Create new TSANPR instance
pub fn new(library_path string) !TSANPR {
	lib_handle := dl.open(library_path, dl.rtld_lazy)
	if lib_handle == unsafe { nil } {
		return error('Failed to load library: ${library_path}')
	}

	anpr_initialize := dl.sym(lib_handle, 'anpr_initialize')
	if anpr_initialize == unsafe { nil } {
		dl.close(lib_handle)
		return error('Failed to load anpr_initialize')
	}

	anpr_read_file := dl.sym(lib_handle, 'anpr_read_file')
	if anpr_read_file == unsafe { nil } {
		dl.close(lib_handle)
		return error('Failed to load anpr_read_file')
	}

	anpr_read_pixels := dl.sym(lib_handle, 'anpr_read_pixels')
	if anpr_read_pixels == unsafe { nil } {
		dl.close(lib_handle)
		return error('Failed to load anpr_read_pixels')
	}

	return TSANPR{
		lib_handle:       lib_handle
		anpr_initialize:  unsafe { FnInitialize(anpr_initialize) }
		anpr_read_file:   unsafe { FnReadFile(anpr_read_file) }
		anpr_read_pixels: unsafe { FnReadPixels(anpr_read_pixels) }
	}
}

// Destroy TSANPR instance
pub fn (mut t TSANPR) destroy() {
	if t.lib_handle != unsafe { nil } {
		dl.close(t.lib_handle)
		t.lib_handle = unsafe { nil }
	}
}

// Initialize ANPR engine
pub fn (t &TSANPR) initialize(mode string) string {
	result := t.anpr_initialize(mode.str)
	return unsafe { cstring_to_vstring(result) }
}

// Read image file
pub fn (t &TSANPR) read_file(img_file_name string, output_format string, options string) string {
	result := t.anpr_read_file(img_file_name.str, output_format.str, options.str)
	return unsafe { cstring_to_vstring(result) }
}

// Read pixel data
pub fn (t &TSANPR) read_pixels(pixels []u8, width u64, height u64, stride i64, pixel_format string, output_format string, options string) string {
	result := t.anpr_read_pixels(pixels.data, width, height, stride, pixel_format.str, output_format.str, options.str)
	return unsafe { cstring_to_vstring(result) }
}
