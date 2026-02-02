# The MIT License (MIT)
# Copyright © 2022-2025 TS-Solution Corp.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to all conditions.
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Source the TSANPR wrapper
source("tsanpr.R")

# Define constants
EXAMPLES_BASE_DIR <- normalizePath(file.path(getwd(), "../.."))

#' Generate engine filename depending on platform and architecture.
#'
#' @return Character string with engine file path
get_engine_file_name <- function() {
  os_name <- Sys.info()["sysname"]
  arch_name <- Sys.info()["machine"]

  if (os_name == "Windows") {
    if (arch_name %in% c("AMD64", "x86_64", "x86-64")) {
      return(file.path(EXAMPLES_BASE_DIR, "bin", "windows-x86_64", "tsanpr.dll"))
    } else if (arch_name %in% c("x86", "i386", "i686")) {
      return(file.path(EXAMPLES_BASE_DIR, "bin", "windows-x86", "tsanpr.dll"))
    }
  } else if (os_name == "Linux") {
    if (arch_name %in% c("x86_64", "amd64")) {
      return(file.path(EXAMPLES_BASE_DIR, "bin", "linux-x86_64", "libtsanpr.so"))
    } else if (arch_name == "aarch64") {
      return(file.path(EXAMPLES_BASE_DIR, "bin", "linux-aarch64", "libtsanpr.so"))
    }
  }

  return("")
}

#' Read an image file and call anpr_read_file.
#'
#' @param tsanpr TSANPR object
#' @param imgfile Path to image file
#' @param output_format Output format (text, json, yaml, xml, csv)
#' @param options Processing options
read_image_file <- function(tsanpr, imgfile, output_format, options) {
  cat(sprintf('%s (outputFormat="%s", options="%s") => ',
              imgfile, output_format, options))

  result <- tsanpr$anpr_read_file(imgfile, output_format, options)
  cat(result, "\n")
}

#' Read an encoded image file as bytes and call anpr_read_pixels with 'encoded' pixel format.
#'
#' @param tsanpr TSANPR object
#' @param imgfile Path to image file
#' @param output_format Output format (text, json, yaml, xml, csv)
#' @param options Processing options
read_encoded_image <- function(tsanpr, imgfile, output_format, options) {
  cat(sprintf('%s (outputFormat="%s", options="%s") => ',
              imgfile, output_format, options))

  tryCatch({
    if (!file.exists(imgfile)) {
      cat("File does not exist\n")
      return()
    }

    # Read file as raw bytes
    encoded_img <- readBin(imgfile, "raw", file.info(imgfile)$size)

    result <- tsanpr$anpr_read_pixels(
      encoded_img,
      length(encoded_img),
      0L,
      0L,
      "encoded",
      output_format,
      options
    )
    cat(result, "\n")

  }, error = function(e) {
    cat("ERROR: Exception -", e$message, "\n")
  })
}

#' Determine pixel format string based on image channels.
#'
#' @param channels Number of color channels
#' @return Character string with pixel format
get_pixel_format <- function(channels) {
  switch(as.character(channels),
    "1" = "GRAY",
    "2" = "BGR565",  # or "BGR555"
    "3" = "BGR",
    "4" = "BGRA",
    ""
  )
}

#' Use the pixel buffer-based ANPR function.
#' This implementation uses the magick package for image processing.
#'
#' @param tsanpr TSANPR object
#' @param imgfile Path to image file
#' @param output_format Output format (text, json, yaml, xml, csv)
#' @param options Processing options
read_pixel_buffer <- function(tsanpr, imgfile, output_format, options) {
  cat(sprintf('%s (outputFormat="%s", options="%s") => ',
              imgfile, output_format, options))

  tryCatch({
    # Check if magick package is available
    if (!requireNamespace("magick", quietly = TRUE)) {
      cat("magick package not available for pixel buffer processing\n")
      return()
    }

    # Read image using magick
    img <- magick::image_read(imgfile)
    img_info <- magick::image_info(img)

    width <- img_info$width
    height <- img_info$height

    # Get raw RGB pixel data directly from magick
    # magick returns [channels, width, height] array
    # R's column-major order converts this to RGB interleaved format automatically
    pixel_data <- magick::image_data(img, channels = "rgb")
    pixel_vector <- as.raw(pixel_data)

    stride <- width * 3L
    result <- tsanpr$anpr_read_pixels(
      pixel_vector,
      width,
      height,
      stride,
      "RGB",
      output_format,
      options
    )
    cat(result, "\n")

  }, error = function(e) {
    cat("ERROR: Exception -", e$message, "\n")
  })
}

#' NOTICE:
#' anpr_initialize should be called only once after library load.
#' Therefore, it is not possible to change the country code after anpr_initialize has been called.
#' While using the free trial license, you can try all languages.
#' When you purchase a commercial license, you can only use the selected language.
#'
#' @param tsanpr TSANPR object
#' @param country_code Country code (KR, JP, VN, etc.)
read_license_plates <- function(tsanpr, country_code) {
  error <- tsanpr$anpr_initialize(paste0("text;country=", country_code))
  if (nchar(error) > 0) {
    cat("anpr_initialize() failed:", error, "\n")
    return()
  }

  image_dir <- file.path(EXAMPLES_BASE_DIR, "img", country_code)

  # TODO: Try each function as needed
  anpr_func <- read_image_file
  # anpr_func <- read_encoded_image
  # anpr_func <- read_pixel_buffer

  # TODO: Try each output format as needed
  output_format <- "text"
  # output_format <- "json"
  # output_format <- "yaml"
  # output_format <- "xml"
  # output_format <- "csv"

  # Single license plate recognition (default)
  anpr_func(tsanpr, file.path(image_dir, "licensePlate.jpg"), output_format, "")

  # Recognize multiple license plates attached to vehicles
  anpr_func(tsanpr, file.path(image_dir, "multiple.jpg"), output_format, "vm")

  # Recognize multiple license plates attached to vehicles (including motorcycles)
  anpr_func(tsanpr, file.path(image_dir, "multiple.jpg"), output_format, "vmb")

  # Recognize multiple license plates attached to vehicles with surround detection
  anpr_func(tsanpr, file.path(image_dir, "surround.jpg"), output_format, "vms")

  # Recognize multiple surrounding objects (vehicles)
  anpr_func(tsanpr, file.path(image_dir, "surround.jpg"), output_format, "dms")

  # Recognize multiple surrounding objects (vehicles) and license plates
  anpr_func(tsanpr, file.path(image_dir, "surround.jpg"), output_format, "dmsr")

  # Recognize multiple surrounding objects and license plates within RoI
  anpr_func(tsanpr, file.path(image_dir, "surround.jpg"), output_format,
           "dmsri549,700,549,2427,1289,2427,1289,700")
}

#' Main function
main <- function() {
  engine_file_name <- get_engine_file_name()
  if (nchar(engine_file_name) == 0 || !file.exists(engine_file_name)) {
    cat("Unsupported operating system or engine not found\n")
    cat("Please extract the TSANPR engine files to the bin directory.\n")
    return()
  }

  tryCatch({
    tsanpr <- TSANPR$new(engine_file_name)

    # TODO: Try each country code as needed
    read_license_plates(tsanpr, "KR")
    # read_license_plates(tsanpr, "JP")
    # read_license_plates(tsanpr, "VN")

  }, error = function(e) {
    cat("TSANPR initialization failed:", e$message, "\n")
    cat("Please compile the R wrapper using: R CMD SHLIB src/tsanpr_r.c\n")
  })
}

# Run the main function
main()
