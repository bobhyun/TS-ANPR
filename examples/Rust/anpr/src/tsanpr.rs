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

use std::os::raw::{c_char, c_uchar, c_ulong, c_long};
use libloading::{Library, Symbol};

pub type AnprInitializeFn = unsafe extern "C" fn(*const c_char) -> *const c_char;
pub type AnprReadFileFn = unsafe extern "C" fn(*const c_char, *const c_char, *const c_char) -> *const c_char;
pub type AnprReadPixelsFn = unsafe extern "C" fn(
    *const c_uchar,
    c_ulong,
    c_ulong,
    c_long,
    *const c_char,
    *const c_char,
    *const c_char,
) -> *const c_char;

pub struct TSANPR {
    pub anpr_initialize: Symbol<'static, AnprInitializeFn>,
    #[allow(dead_code)]
    pub anpr_read_file: Symbol<'static, AnprReadFileFn>,
    #[allow(dead_code)]
    pub anpr_read_pixels: Symbol<'static, AnprReadPixelsFn>,
    _lib: &'static Library,
}

impl TSANPR {
    pub unsafe fn load(engine_file_name: &str) -> Result<Self, String> {
        let lib = Box::leak(Box::new(unsafe { Library::new(engine_file_name) }
            .map_err(|e| format!("Cannot load module: {}", e))?));

        let anpr_initialize: Symbol<AnprInitializeFn> =
            unsafe { lib.get(b"anpr_initialize") }.map_err(|_| "anpr_initialize() not found.".to_string())?;
        let anpr_read_file: Symbol<AnprReadFileFn> =
            unsafe { lib.get(b"anpr_read_file") }.map_err(|_| "anpr_read_file() not found.".to_string())?;
        let anpr_read_pixels: Symbol<AnprReadPixelsFn> =
            unsafe { lib.get(b"anpr_read_pixels") }.map_err(|_| "anpr_read_pixels() not found.".to_string())?;

        Ok(TSANPR {
            anpr_initialize: unsafe { std::mem::transmute(anpr_initialize) },
            anpr_read_file: unsafe { std::mem::transmute(anpr_read_file) },
            anpr_read_pixels: unsafe { std::mem::transmute(anpr_read_pixels) },
            _lib: lib,
        })
    }
}
