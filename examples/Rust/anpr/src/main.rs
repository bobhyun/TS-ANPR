/*
The MIT License (MIT)
Copyright © 2022-2025 TS-Solution Corp.
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to all conditions.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

mod tsanpr;
use crate::tsanpr::TSANPR;
use std::os::raw::{c_ulong, c_long};
use std::ffi::{CString, CStr};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use image::{ImageReader, DynamicImage};

static EXAMPLES_BASE_DIR: &str = "../..";

// Generate engine filename depending on platform
fn get_engine_file_name() -> String {
    let base = PathBuf::from(EXAMPLES_BASE_DIR);
    if cfg!(target_os = "windows") && cfg!(target_pointer_width = "64") {
        base.join("bin/windows-x86_64/tsanpr.dll").to_string_lossy().to_string()
    } else if cfg!(target_os = "windows") {
        base.join("bin/windows-x86/tsanpr.dll").to_string_lossy().to_string()
    } else if cfg!(target_os = "linux") && cfg!(target_pointer_width = "64") {
        base.join("bin/linux-x86_64/libtsanpr.so").to_string_lossy().to_string()
    } else if cfg!(target_os = "linux") && cfg!(target_arch = "aarch64") {
        base.join("bin/linux-aarch64/libtsanpr.so").to_string_lossy().to_string()
    } else {
        String::new()
    }
}

// Read image file using engine
#[allow(dead_code)]
fn read_image_file(tsanpr: &TSANPR, imgfile: &str, output_format: &str, options: &str) {
    println!("{} (outputFormat=\"{}\", options=\"{}\") =>", imgfile, output_format, options);
    let imgfile_c = CString::new(imgfile).unwrap();
    let output_format_c = CString::new(output_format).unwrap();
    let options_c = CString::new(options).unwrap();
    unsafe {
        let result_ptr = (tsanpr.anpr_read_file)(imgfile_c.as_ptr(), output_format_c.as_ptr(), options_c.as_ptr());
        if !result_ptr.is_null() {
            let result = CStr::from_ptr(result_ptr).to_string_lossy();
            println!("{}", result);
        }
    }
}

// Read encoded image as a buffer
#[allow(dead_code)]
fn read_encoded_image(tsanpr: &TSANPR, imgfile: &str, output_format: &str, options: &str) {
    println!("{} (outputFormat=\"{}\", options=\"{}\") =>", imgfile, output_format, options);
    let mut file = match File::open(imgfile) {
        Ok(f) => f,
        Err(_) => {
            println!("File open failed");
            return;
        }
    };
    let mut encoded_img = Vec::new();
    if file.read_to_end(&mut encoded_img).is_err() {
        println!("File read failed");
        return;
    }
    let output_format_c = CString::new(output_format).unwrap();
    let options_c = CString::new(options).unwrap();
    let encoded_c = CString::new("encoded").unwrap();
    unsafe {
        let result_ptr = (tsanpr.anpr_read_pixels)(
            encoded_img.as_ptr(),
            encoded_img.len() as c_ulong,
            0,
            0,
            encoded_c.as_ptr(),
            output_format_c.as_ptr(),
            options_c.as_ptr(),
        );
        if !result_ptr.is_null() {
            let result = CStr::from_ptr(result_ptr).to_string_lossy();
            println!("{}", result);
        }
    }
}

// Read decoded pixel buffer (stub, real implementation needs image crate or opencv)
#[allow(dead_code)]
fn read_pixel_buffer(tsanpr: &TSANPR, imgfile: &str, output_format: &str, options: &str) {
println!("{} (outputFormat=\"{}\", options=\"{}\") =>", imgfile, output_format, options);

    let img = match ImageReader::open(imgfile) {
        Ok(reader) => match reader.decode() {
            Ok(img) => img,
            Err(e) => {
                eprintln!("Image decode failed! ({})", e);
                return;
            }
        },
        Err(e) => {
            eprintln!("Image open failed! ({})", e);
            return;
        }
    };

    let (pixel_format, buffer, width, height, stride): (&str, &[u8], u32, u32, usize) = match &img {
        DynamicImage::ImageLuma8(buf) => (
            "GRAY",
            buf.as_raw(),
            buf.width(),
            buf.height(),
            buf.sample_layout().height_stride,
        ),
        DynamicImage::ImageRgb8(buf) => (
            "RGB",
            buf.as_raw(),
            buf.width(),
            buf.height(),
            buf.sample_layout().height_stride,
        ),
        DynamicImage::ImageRgba8(buf) => (
            "RGBA",
            buf.as_raw(),
            buf.width(),
            buf.height(),
            buf.sample_layout().height_stride,
        ),
        _ => {
            eprintln!("Unsupported pixel format!");
            return;
        }
    };

    let pixel_format_c = CString::new(pixel_format).unwrap();
    let output_format_c = CString::new(output_format).unwrap();
    let options_c = CString::new(options).unwrap();

    unsafe {
        let result_ptr = (tsanpr.anpr_read_pixels)(
            buffer.as_ptr(),
            width as c_ulong,
            height as c_ulong,
            stride as c_long,
            pixel_format_c.as_ptr(),
            output_format_c.as_ptr(),
            options_c.as_ptr(),
        );
        if !result_ptr.is_null() {
            let result = std::ffi::CStr::from_ptr(result_ptr).to_string_lossy();
            println!("{}", result);
        }
    }
}

// Recognize license plates for a given country
fn read_license_plates(tsanpr: &TSANPR, country_code: &str) -> i32 {
    // NOTICE:
    // anpr_initialize should be called only once after library load.
    // Therefore, it is not possible to change the country code after anpr_initialize has been called.
    // While using the free trial license, you can try all languages.
    // When you purchase a commercial license, you can only use the selected language.
    let init_params = format!("text;country={}", country_code);
    let init_params_c = CString::new(init_params).unwrap();
    unsafe {
        let error_ptr = (tsanpr.anpr_initialize)(init_params_c.as_ptr());
        if !error_ptr.is_null() && CStr::from_ptr(error_ptr).to_bytes().len() > 0 {
            let error = CStr::from_ptr(error_ptr).to_string_lossy();
            println!("anpr_initialize() failed (error={})", error);
            return -1;
        }
    }
    let image_dir = PathBuf::from(format!("{}/img/{}/", EXAMPLES_BASE_DIR, country_code));
    let image_dir_str = image_dir.to_string_lossy();

    // TODO: Try each function as needed
    let anpr_func = read_image_file as fn(&TSANPR, &str, &str, &str);
    // let anpr_func = read_encoded_image as fn(&TSANPR, &str, &str, &str);
    // let anpr_func = read_pixel_buffer as fn(&TSANPR, &str, &str, &str);

    // TODO: Try each output format as needed
    let output_format = "text";
    // let output_format = "json";
    // let output_format = "yaml";
    // let output_format = "xml";
    // let output_format = "csv";

    anpr_func(tsanpr, &format!("{}licensePlate.jpg", image_dir_str), output_format, "");    // Single license plate recognition (default)
    anpr_func(tsanpr, &format!("{}multiple.jpg", image_dir_str), output_format, "vm");      // Recognize multiple license plates attached to vehicles
    anpr_func(tsanpr, &format!("{}multiple.jpg", image_dir_str), output_format, "vmb");     // Recognize multiple license plates including motorcycles
    anpr_func(tsanpr, &format!("{}surround.jpg", image_dir_str), output_format, "vms");     // Recognize multiple license plates with surround detection
    anpr_func(tsanpr, &format!("{}surround.jpg", image_dir_str), output_format, "dms");     // Recognize multiple surrounding objects (vehicles)
    anpr_func(tsanpr, &format!("{}surround.jpg", image_dir_str), output_format, "dmsr");    // Recognize multiple surrounding objects and license plates

    0
}

fn main() {
    let engine_file_name = get_engine_file_name();
    let tsanpr = unsafe {
        match TSANPR::load(&engine_file_name) {
            Ok(t) => t,
            Err(e) => {
                println!("{}", e);
                return;
            }
        }
    };

    // TODO: Try each country code as needed
    read_license_plates(&tsanpr, "KR");
    // read_license_plates(&tsanpr, "JP");
    // read_license_plates(&tsanpr, "VN");

    // TSANPR_unload() is not needed, as Library will be dropped automatically
}
