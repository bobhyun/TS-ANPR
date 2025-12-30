// The MIT License (MIT)
// Copyright © 2022-2025 TS-Solution Corp.

import gleam/io
import gleam/dynamic
import tsanpr

// Base directory for engine files
const examples_base_dir = "../.."

// Get the appropriate engine file path based on platform
fn get_engine_file_name() -> String {
  get_engine_path_erl()
}

// Read image file using engine
pub fn read_image_file(
  tsanpr_instance: tsanpr.TSANPR,
  imgfile: String,
  output_format: String,
  options: String,
) -> Result(String, String) {
  io.print(imgfile <> " (outputFormat=\"" <> output_format <> "\", options=\"" <> options <> "\") => ")
  case tsanpr.read_file(tsanpr_instance, imgfile, output_format, options) {
    Ok(result) -> {
      io.println(result)
      Ok(result)
    }
    Error(err) -> {
      io.println("Error: " <> err)
      Error(err)
    }
  }
}

// Read encoded image as a buffer
pub fn read_encoded_image(
  tsanpr_instance: tsanpr.TSANPR,
  imgfile: String,
  output_format: String,
  options: String,
) -> Result(String, String) {
  io.print(imgfile <> " (outputFormat=\"" <> output_format <> "\", options=\"" <> options <> "\") => ")
  case read_file_binary(imgfile) {
    Ok(encoded_img) -> {
      let size = binary_size(encoded_img)
      case tsanpr.read_pixels_binary(tsanpr_instance, encoded_img, size, 0, 0, "encoded", output_format, options) {
        Ok(result) -> {
          io.println(result)
          Ok(result)
        }
        Error(err) -> {
          io.println("Error: " <> err)
          Error(err)
        }
      }
    }
    Error(_) -> {
      io.println("File read failed")
      Error("File read failed")
    }
  }
}

// Read pixel buffer - decode image to raw pixels and process
pub fn read_pixel_buffer(
  tsanpr_instance: tsanpr.TSANPR,
  imgfile: String,
  output_format: String,
  options: String,
) -> Result(String, String) {
  io.print(imgfile <> " (outputFormat=\"" <> output_format <> "\", options=\"" <> options <> "\") => ")
  case tsanpr.decode_image(imgfile) {
    Ok(decoded) -> {
      let stride = decoded.width * decoded.channels
      case tsanpr.read_pixels_binary(tsanpr_instance, decoded.pixels, decoded.width, decoded.height, stride, "RGB", output_format, options) {
        Ok(result) -> {
          io.println(result)
          Ok(result)
        }
        Error(err) -> {
          io.println("Error: " <> err)
          Error(err)
        }
      }
    }
    Error(err) -> {
      io.println("Error decoding image: " <> err)
      Error(err)
    }
  }
}

// Recognize license plates for a given country
fn read_license_plates(
  tsanpr_instance: tsanpr.TSANPR,
  country_code: String,
) -> Result(Nil, String) {
  // NOTICE:
  // anpr_initialize should be called only once after library load.
  // Therefore, it is not possible to change the country code after anpr_initialize has been called.
  // While using the free trial license, you can try all languages.
  // When you purchase a commercial license, you can only use the selected language.
  let init_params = "text;country=" <> country_code
  case tsanpr.initialize(tsanpr_instance, init_params) {
    Ok("") -> {
      let image_dir = examples_base_dir <> "/img/" <> country_code

      // TODO: Try each function as needed
      let anpr_func = read_image_file
      // let anpr_func = read_encoded_image
      // let anpr_func = read_pixel_buffer

      // TODO: Try each output format as needed
      let output_format = "text"
      // let output_format = "json"
      // let output_format = "yaml"
      // let output_format = "xml"
      // let output_format = "csv"

      let _ = anpr_func(tsanpr_instance, image_dir <> "/licensePlate.jpg", output_format, "")    // Single license plate recognition (default)
      let _ = anpr_func(tsanpr_instance, image_dir <> "/multiple.jpg", output_format, "vm")      // Recognize multiple license plates attached to vehicles
      let _ = anpr_func(tsanpr_instance, image_dir <> "/multiple.jpg", output_format, "vmb")     // Recognize multiple license plates attached to vehicles (including motorcycles)
      let _ = anpr_func(tsanpr_instance, image_dir <> "/surround.jpg", output_format, "vms")     // Recognize multiple license plates attached to vehicles with surround detection
      let _ = anpr_func(tsanpr_instance, image_dir <> "/surround.jpg", output_format, "dms")     // Recognize multiple surrounding objects (vehicles)
      let _ = anpr_func(tsanpr_instance, image_dir <> "/surround.jpg", output_format, "dmsr")    // Recognize multiple surrounding objects (vehicles) and license plates

      // Recognize multiple surrounding objects and license plates within RoI
      let _ = anpr_func(tsanpr_instance, image_dir <> "/surround.jpg", output_format, "dmsri549,700,549,2427,1289,2427,1289,700")
      Ok(Nil)
    }
    Ok(error) -> {
      io.println("anpr_initialize() failed: " <> error)
      Error(error)
    }
    Error(err) -> {
      io.println("anpr_initialize() failed: " <> err)
      Error(err)
    }
  }
}

pub fn main() {
  let engine_file_name = get_engine_file_name()
  io.println("Engine file path: " <> engine_file_name)
  case file_exists(engine_file_name) {
    False -> {
      io.println("Unsupported operating system or engine not found: " <> engine_file_name)
    }
    True -> {
      case tsanpr.new(engine_file_name) {
        Ok(tsanpr_instance) -> {
          // TODO: Try each country code as needed
          let _ = read_license_plates(tsanpr_instance, "KR")
          // let _ = read_license_plates(tsanpr_instance, "JP")
          // let _ = read_license_plates(tsanpr_instance, "VN")
          Nil
        }
        Error(err) -> {
          io.println("TSANPR initialization failed: " <> err)
        }
      }
    }
  }
}

@external(erlang, "anpr_ffi", "file_exists")
fn file_exists(path: String) -> Bool

@external(erlang, "anpr_ffi", "get_engine_path")
fn get_engine_path_erl() -> String

@external(erlang, "anpr_ffi", "read_file_binary")
fn read_file_binary(path: String) -> Result(dynamic.Dynamic, Nil)

@external(erlang, "erlang", "byte_size")
fn binary_size(binary: dynamic.Dynamic) -> Int
