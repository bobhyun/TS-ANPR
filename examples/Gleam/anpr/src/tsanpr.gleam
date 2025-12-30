// The MIT License (MIT)
// Copyright © 2022-2025 TS-Solution Corp.

import gleam/dynamic.{type Dynamic}

/// An opaque type representing an instance of the TS-ANPR engine.
/// Internally holds the NIF resource handle.
pub type TSANPR =
  Dynamic

/// Creates a new TS-ANPR engine instance.
///
/// Parameters:
///   - `path`: The file path to the `tsanpr.dll` or `libtsanpr.so` library.
///
/// Returns:
///   A `Result` containing the `TSANPR` instance on success, or an error string.
///
pub fn new(path: String) -> Result(TSANPR, String) {
  load_nif(path)
}

/// Initializes the ANPR engine.
/// This must be called once before any recognition functions are used.
///
/// Parameters:
///   - `tsanpr`: The `TSANPR` instance.
///   - `options`: Initialization options string (e.g., "text;country=KR").
///
/// Returns:
///   An empty string on success, or an error message string.
///
pub fn initialize(tsanpr: TSANPR, options: String) -> Result(String, String) {
  Ok(anpr_initialize_nif(tsanpr, options))
}

/// Reads license plates from an image file.
///
/// Parameters:
///   - `tsanpr`: The `TSANPR` instance.
///   - `filepath`: Path to the image file.
///   - `output_format`: The desired output format ("text", "json", "xml", etc.).
///   - `options`: Recognition options string.
///
/// Returns:
///   A `Result` containing the recognition result string on success, or an error.
///
pub fn read_file(
  tsanpr: TSANPR,
  filepath: String,
  output_format: String,
  options: String,
) -> Result(String, String) {
  Ok(anpr_read_file_nif(tsanpr, filepath, output_format, options))
}

/// Reads license plates from a raw pixel buffer (as a binary).
///
/// Parameters:
///   - `tsanpr`: The `TSANPR` instance.
///   - `pixels`: The image data as a binary.
///   - `width`: Image width.
///   - `height`: Image height.
///   - `stride`: Image stride (row size in bytes).
///   - `pixel_format`: The pixel format ("encoded", "BGR", "GRAY", etc.).
///   - `output_format`: The desired output format.
///   - `options`: Recognition options.
///
/// Returns:
///   A `Result` containing the recognition result string on success, or an error.
///
pub fn read_pixels_binary(
  tsanpr: TSANPR,
  pixels: Dynamic,
  width: Int,
  height: Int,
  stride: Int,
  pixel_format: String,
  output_format: String,
  options: String,
) -> Result(String, String) {
  Ok(anpr_read_pixels_nif(
    tsanpr,
    pixels,
    width,
    height,
    stride,
    pixel_format,
    output_format,
    options,
  ))
}

//
// FFI (Foreign Function Interface) definitions - calls to tsanpr_ffi Erlang module
//

@external(erlang, "tsanpr_ffi", "load")
fn load_nif(path: String) -> Result(Dynamic, String)

@external(erlang, "tsanpr_ffi", "anpr_initialize")
fn anpr_initialize_nif(resource: Dynamic, options: String) -> String

@external(erlang, "tsanpr_ffi", "anpr_read_file")
fn anpr_read_file_nif(
  resource: Dynamic,
  filepath: String,
  output_format: String,
  options: String,
) -> String

@external(erlang, "tsanpr_ffi", "anpr_read_pixels")
fn anpr_read_pixels_nif(
  resource: Dynamic,
  pixels: Dynamic,
  width: Int,
  height: Int,
  stride: Int,
  pixel_format: String,
  output_format: String,
  options: String,
) -> String

/// Decoded image data containing pixel buffer and dimensions
pub type DecodedImage {
  DecodedImage(pixels: Dynamic, width: Int, height: Int, channels: Int)
}

/// Decodes an image file into raw RGB pixel data.
///
/// Parameters:
///   - `filepath`: Path to the image file (JPEG, PNG, BMP, etc.).
///
/// Returns:
///   A `Result` containing `DecodedImage` on success, or an error string.
///
pub fn decode_image(filepath: String) -> Result(DecodedImage, String) {
  decode_image_nif(filepath)
}

@external(erlang, "tsanpr_ffi", "decode_image")
fn decode_image_nif(filepath: String) -> Result(DecodedImage, String)
