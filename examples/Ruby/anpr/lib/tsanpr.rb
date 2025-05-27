=begin
The MIT License (MIT)
Copyright © 2022-2025 TS-Solution Corp.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to all conditions.

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
=end

require 'ffi'

class TSANPR
  extend FFI::Library

  ffi_lib ENV['TSANPR_LIB_PATH'] || 'libtsanpr.so'

  # Change the attached function name to avoid collision
  attach_function :c_anpr_initialize, :anpr_initialize, [:string], :pointer
  attach_function :c_anpr_read_file, :anpr_read_file, [:string, :string, :string], :pointer
  attach_function :c_anpr_read_pixels, :anpr_read_pixels, [:pointer, :uint64, :uint64, :int64, :string, :string, :string], :pointer

  def anpr_initialize(mode)
    mode_utf8 = mode.encode('UTF-8')
    ptr = TSANPR.c_anpr_initialize(mode_utf8)
    return "" if ptr.null?
    ptr.read_string || ""
  end

  def anpr_read_file(img_file_name, output_format, options)
    img_utf8 = img_file_name.encode('UTF-8')
    out_utf8 = output_format.encode('UTF-8')
    opt_utf8 = options.encode('UTF-8')
    ptr = TSANPR.c_anpr_read_file(img_utf8, out_utf8, opt_utf8)
    return "" if ptr.null?
    ptr.read_string || ""
  end

  def anpr_read_pixels(pixels, width, height, stride, pixel_format, output_format, options)
    pf_utf8 = pixel_format.encode('UTF-8')
    out_utf8 = output_format.encode('UTF-8')
    opt_utf8 = options.encode('UTF-8')
    ptr = TSANPR.c_anpr_read_pixels(pixels, width, height, stride, pf_utf8, out_utf8, opt_utf8)
    return "" if ptr.null?
    ptr.read_string || ""
  end
end
