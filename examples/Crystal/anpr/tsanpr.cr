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

{% if flag?(:win32) %}
  @[Link("kernel32")]
  lib LibC
    fun LoadLibraryW(lpLibFileName : UInt16*) : Void*
    fun GetProcAddress(hModule : Void*, lpProcName : UInt8*) : Void*
  end
{% elsif flag?(:linux) %}
  @[Link("dl")]
  lib LibC
    fun dlopen(filename : UInt8*, flag : Int32) : Void*
    fun dlsym(handle : Void*, symbol : UInt8*) : Void*
    fun dlerror() : UInt8*
  end
{% end %}

lib LibTSANPR
  fun anpr_initialize(mode : UInt8*) : UInt8*
  fun anpr_read_file(img_file_name : UInt8*, output_format : UInt8*, options : UInt8*) : UInt8*
  fun anpr_read_pixels(pixels : UInt8*, width : UInt64, height : UInt64, stride : Int64,
                      pixel_format : UInt8*, output_format : UInt8*, options : UInt8*) : UInt8*
end

class TSANPR
  @handle : Void*?
  @anpr_initialize_func : Proc(UInt8*, UInt8*)?
  @anpr_read_file_func : Proc(UInt8*, UInt8*, UInt8*, UInt8*)?
  @anpr_read_pixels_func : Proc(UInt8*, UInt64, UInt64, Int64, UInt8*, UInt8*, UInt8*, UInt8*)?

  def initialize(library_path : String)
    # Initialize TSANPR with the given library path.
    unless File.exists?(library_path)
      raise "Library file not found: #{library_path}"
    end

    {% if flag?(:win32) %}
      # On Windows, use LoadLibraryW and GetProcAddress
      wide_path = library_path.to_utf16
      @handle = LibC.LoadLibraryW(wide_path)
      if @handle.nil?
        raise "Failed to load library: #{library_path}"
      end

      # Load function pointers
      init_ptr = LibC.GetProcAddress(@handle.not_nil!, "anpr_initialize")
      read_file_ptr = LibC.GetProcAddress(@handle.not_nil!, "anpr_read_file")
      read_pixels_ptr = LibC.GetProcAddress(@handle.not_nil!, "anpr_read_pixels")

      if init_ptr.nil? || read_file_ptr.nil? || read_pixels_ptr.nil?
        raise "Failed to load ANPR functions from library"
      end

      @anpr_initialize_func = Proc(UInt8*, UInt8*).new(init_ptr, Pointer(Void).null)
      @anpr_read_file_func = Proc(UInt8*, UInt8*, UInt8*, UInt8*).new(read_file_ptr, Pointer(Void).null)
      @anpr_read_pixels_func = Proc(UInt8*, UInt64, UInt64, Int64, UInt8*, UInt8*, UInt8*, UInt8*).new(read_pixels_ptr, Pointer(Void).null)
    {% elsif flag?(:linux) %}
      # On Linux, use dlopen and dlsym
      # RTLD_NOW = 0x00002
      @handle = LibC.dlopen(library_path, 0x00002)
      if @handle.nil?
        error = LibC.dlerror()
        error_msg = error.null? ? "Unknown error" : String.new(error)
        raise "Failed to load library: #{library_path} - #{error_msg}"
      end

      # Load function pointers
      init_ptr = LibC.dlsym(@handle.not_nil!, "anpr_initialize")
      read_file_ptr = LibC.dlsym(@handle.not_nil!, "anpr_read_file")
      read_pixels_ptr = LibC.dlsym(@handle.not_nil!, "anpr_read_pixels")

      if init_ptr.nil? || read_file_ptr.nil? || read_pixels_ptr.nil?
        error = LibC.dlerror()
        error_msg = error.null? ? "Unknown error" : String.new(error)
        raise "Failed to load ANPR functions from library: #{error_msg}"
      end

      @anpr_initialize_func = Proc(UInt8*, UInt8*).new(init_ptr, Pointer(Void).null)
      @anpr_read_file_func = Proc(UInt8*, UInt8*, UInt8*, UInt8*).new(read_file_ptr, Pointer(Void).null)
      @anpr_read_pixels_func = Proc(UInt8*, UInt64, UInt64, Int64, UInt8*, UInt8*, UInt8*, UInt8*).new(read_pixels_ptr, Pointer(Void).null)
    {% else %}
      raise "Unsupported platform"
    {% end %}
  end
  
  def anpr_initialize(mode : String) : String
    # Initialize the ANPR engine with the specified mode.
    begin
      func = @anpr_initialize_func
      return "Function not loaded" if func.nil?
      result_ptr = func.call(mode.to_unsafe)

      if result_ptr.null?
        ""
      else
        String.new(result_ptr)
      end
    rescue ex
      "Function call failed: #{ex.message}"
    end
  end

  def anpr_read_file(img_file_name : String, output_format : String, options : String) : String
    # Read and process an image file.
    begin
      func = @anpr_read_file_func
      return "Function not loaded" if func.nil?
      result_ptr = func.call(
        img_file_name.to_unsafe,
        output_format.to_unsafe,
        options.to_unsafe
      )

      if result_ptr.null?
        ""
      else
        String.new(result_ptr)
      end
    rescue ex
      "Function call failed: #{ex.message}"
    end
  end

  def anpr_read_pixels(pixels : UInt8*, width : UInt64, height : UInt64, stride : Int64,
                      pixel_format : String, output_format : String, options : String) : String
    # Process pixel data directly.
    begin
      func = @anpr_read_pixels_func
      return "Function not loaded" if func.nil?
      result_ptr = func.call(
        pixels,
        width,
        height,
        stride,
        pixel_format.to_unsafe,
        output_format.to_unsafe,
        options.to_unsafe
      )

      if result_ptr.null?
        ""
      else
        String.new(result_ptr)
      end
    rescue ex
      "Function call failed: #{ex.message}"
    end
  end
end