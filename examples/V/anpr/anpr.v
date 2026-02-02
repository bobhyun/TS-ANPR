// The MIT License (MIT)
// Copyright © 2022-2025 TS-Solution Corp.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to all conditions.
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import os
import stbi
import tsanpr

const examples_base_dir = '../../'

// Generate engine filename depending on platform and architecture
fn get_engine_file_name() string {
	$if windows {
		$if amd64 {
			return os.join_path(examples_base_dir, 'bin', 'windows-x86_64', 'tsanpr.dll')
		} $else $if x32 {
			return os.join_path(examples_base_dir, 'bin', 'windows-x86', 'tsanpr.dll')
		}
	} $else $if linux {
		$if amd64 {
			return os.join_path(examples_base_dir, 'bin', 'linux-x86_64', 'libtsanpr.so')
		} $else $if arm64 {
			return os.join_path(examples_base_dir, 'bin', 'linux-aarch64', 'libtsanpr.so')
		}
	}
	return ''
}

// Read an image file and call anpr_read_file
fn read_image_file(t &tsanpr.TSANPR, imgfile string, output_format string, options string) {
	print('${imgfile} (outputFormat="${output_format}", options="${options}") => ')

	if !os.exists(imgfile) {
		println('File does not exist')
		return
	}

	result := t.read_file(imgfile, output_format, options)
	println(result)
}

// Read an encoded image file as bytes and call anpr_read_pixels with 'encoded' pixel format
fn read_encoded_image(t &tsanpr.TSANPR, imgfile string, output_format string, options string) {
	print('${imgfile} (outputFormat="${output_format}", options="${options}") => ')

	if !os.exists(imgfile) {
		println('File does not exist')
		return
	}

	encoded_img := os.read_bytes(imgfile) or {
		println('Failed to read file')
		return
	}

	result := t.read_pixels(encoded_img, u64(encoded_img.len), 0, 0, 'encoded', output_format,
		options)
	println(result)
}

// Determine pixel format string based on image channels
fn get_pixel_format(channels int) string {
	return match channels {
		1 { 'GRAY' }
		3 { 'RGB' }
		4 { 'RGBA' }
		else { '' }
	}
}

// Use the pixel buffer-based ANPR function using stbi module
fn read_pixel_buffer(t &tsanpr.TSANPR, imgfile string, output_format string, options string) {
	print('${imgfile} (outputFormat="${output_format}", options="${options}") => ')

	if !os.exists(imgfile) {
		println('File does not exist')
		return
	}

	// Load image using stbi (stb_image)
	img := stbi.load(imgfile) or {
		println('Failed to load image: ${err}')
		return
	}
	defer {
		img.free()
	}

	width := u64(img.width)
	height := u64(img.height)
	channels := img.nr_channels
	pixel_format := get_pixel_format(channels)

	if pixel_format.len == 0 {
		println('Unsupported image format: ${channels} channels')
		return
	}

	// stbi returns pixel data directly as bytes
	pixel_data := unsafe { img.data.vbytes(int(width * height) * channels) }
	stride := i64(width) * channels

	result := t.read_pixels(pixel_data, width, height, stride, pixel_format, output_format,
		options)
	println(result)
}

// NOTICE:
// anpr_initialize should be called only once after library load.
// Therefore, it is not possible to change the country code after anpr_initialize has been called.
// While using the free trial license, you can try all languages.
// When you purchase a commercial license, you can only use the selected language.
fn read_license_plates(t &tsanpr.TSANPR, country_code string) {
	error := t.initialize('text;country=${country_code}')
	if error.len > 0 {
		println('anpr_initialize() failed: ${error}')
		return
	}

	image_dir := os.join_path(examples_base_dir, 'img', country_code)

	// TODO: Try each function as needed
	anpr_func := read_image_file
	// anpr_func := read_encoded_image
	// anpr_func := read_pixel_buffer

	// TODO: Try each output format as needed
	output_format := 'text'
	// output_format := 'json'
	// output_format := 'yaml'
	// output_format := 'xml'
	// output_format := 'csv'

	// Single license plate recognition (default)
	anpr_func(t, os.join_path(image_dir, 'licensePlate.jpg'), output_format, '')

	// Recognize multiple license plates attached to vehicles
	anpr_func(t, os.join_path(image_dir, 'multiple.jpg'), output_format, 'vm')

	// Recognize multiple license plates attached to vehicles (including motorcycles)
	anpr_func(t, os.join_path(image_dir, 'multiple.jpg'), output_format, 'vmb')

	// Recognize multiple license plates attached to vehicles with surround detection
	anpr_func(t, os.join_path(image_dir, 'surround.jpg'), output_format, 'vms')

	// Recognize multiple surrounding objects (vehicles)
	anpr_func(t, os.join_path(image_dir, 'surround.jpg'), output_format, 'dms')

	// Recognize multiple surrounding objects (vehicles) and license plates
	anpr_func(t, os.join_path(image_dir, 'surround.jpg'), output_format, 'dmsr')

	// Recognize multiple surrounding objects and license plates within RoI
	anpr_func(t, os.join_path(image_dir, 'surround.jpg'), output_format, 'dmsri549,700,549,2427,1289,2427,1289,700')
}

fn main() {
	engine_file_name := get_engine_file_name()
	if engine_file_name.len == 0 || !os.exists(engine_file_name) {
		println('Unsupported operating system or engine not found')
		println('Please extract the TSANPR engine files to the bin directory.')
		return
	}

	mut tsanpr_instance := tsanpr.new(engine_file_name) or {
		println('TSANPR initialization failed: ${err}')
		return
	}
	defer {
		tsanpr_instance.destroy()
	}

	// TODO: Try each country code as needed
	read_license_plates(&tsanpr_instance, 'KR')
	// read_license_plates(&tsanpr_instance, 'JP')
	// read_license_plates(&tsanpr_instance, 'VN')
}
