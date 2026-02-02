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

#' TSANPR R6 Class
#'
#' R wrapper for the TSANPR (TS-Solution Automatic Number Plate Recognition) library.
#' This class provides an interface to the native TSANPR library using a C wrapper
#' that handles dynamic library loading.
#'
#' @importFrom R6 R6Class
#' @export
TSANPR <- R6::R6Class(
  "TSANPR",

  private = list(
    .wrapper_loaded = FALSE,
    .library_path = NULL,

    #' Clean up resources
    finalize = function() {
      tryCatch({
        .Call("tsanpr_unload")
      }, error = function(e) {
        # Ignore cleanup errors
      })
    }
  ),

  public = list(
    #' Initialize TSANPR with the given library path.
    #'
    #' @param library_path Character string with path to the TSANPR library
    initialize = function(library_path) {
      if (!file.exists(library_path)) {
        stop(paste("Library file not found:", library_path))
      }

      private$.library_path <- normalizePath(library_path)

      # Load the R wrapper shared library if not already loaded
      if (!private$.wrapper_loaded) {
        wrapper_path <- self$get_wrapper_path()
        if (!file.exists(wrapper_path)) {
          stop(paste("R wrapper library not found:", wrapper_path,
                     "\nPlease compile using: R CMD SHLIB src/tsanpr_r.c"))
        }

        tryCatch({
          dyn.load(wrapper_path)
          private$.wrapper_loaded <- TRUE
        }, error = function(e) {
          stop(paste("Failed to load R wrapper:", e$message))
        })
      }

      # Load the TSANPR library
      result <- .Call("tsanpr_load", private$.library_path)

      if (result != 0) {
        if (result == -1) {
          stop(paste("Failed to load TSANPR library:", private$.library_path))
        } else if (result == -2) {
          stop("Failed to find required symbols in TSANPR library")
        }
      }
    },

    #' Get the path to the R wrapper shared library.
    #'
    #' @return Character string with wrapper library path
    get_wrapper_path = function() {
      # Determine the correct extension for the platform
      ext <- if (.Platform$OS.type == "windows") ".dll" else ".so"

      # Try different possible locations
      candidates <- c(
        file.path(getwd(), "src", paste0("tsanpr_r", ext)),
        file.path(getwd(), paste0("tsanpr_r", ext)),
        file.path(dirname(sys.frame(1)$ofile %||% ""), "src", paste0("tsanpr_r", ext)),
        file.path(dirname(dirname(getwd())), "R", "anpr", "src", paste0("tsanpr_r", ext))
      )

      for (path in candidates) {
        if (file.exists(path)) {
          return(normalizePath(path))
        }
      }

      # Return default path
      return(file.path(getwd(), "src", paste0("tsanpr_r", ext)))
    },

    #' Initialize the ANPR engine with the specified mode.
    #'
    #' @param mode Character string with initialization mode (e.g., "text;country=KR")
    #' @return Character string with error message if initialization failed, empty string if successful
    anpr_initialize = function(mode) {
      result <- .Call("tsanpr_initialize", as.character(mode))
      return(result)
    },

    #' Read and process an image file.
    #'
    #' @param img_file_name Character string with path to the image file
    #' @param output_format Character string with output format (text, json, yaml, xml, csv)
    #' @param options Character string with processing options
    #' @return Character string with recognition result
    anpr_read_file = function(img_file_name, output_format, options) {
      result <- .Call("tsanpr_read_file",
                     as.character(img_file_name),
                     as.character(output_format),
                     as.character(options))
      return(result)
    },

    #' Process pixel data directly.
    #'
    #' @param pixels Raw vector with pixel data
    #' @param width Integer with image width
    #' @param height Integer with image height
    #' @param stride Integer with row stride in bytes
    #' @param pixel_format Character string with pixel format (RGB, BGR, BGRA, GRAY, etc.)
    #' @param output_format Character string with output format (text, json, yaml, xml, csv)
    #' @param options Character string with processing options
    #' @return Character string with recognition result
    anpr_read_pixels = function(pixels, width, height, stride, pixel_format, output_format, options) {
      # Ensure pixels is a raw vector
      if (!is.raw(pixels)) {
        pixels <- as.raw(pixels)
      }

      result <- .Call("tsanpr_read_pixels",
                     pixels,
                     as.integer(width),
                     as.integer(height),
                     as.integer(stride),
                     as.character(pixel_format),
                     as.character(output_format),
                     as.character(options))
      return(result)
    },

    #' Check if the library is loaded.
    #'
    #' @return Logical indicating whether library is loaded
    is_loaded = function() {
      return(.Call("tsanpr_is_loaded"))
    }
  )
)

# Null-coalescing operator for R
`%||%` <- function(x, y) if (is.null(x)) y else x
