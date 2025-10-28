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

require "./tsanpr"
require "stumpy_png"
require "stumpy_jpeg"

EXAMPLES_BASE_DIR = File.expand_path("../..", Dir.current)

def get_engine_file_name : String
  # Generate engine filename depending on platform and architecture.
  {% if flag?(:win32) %}
    {% if flag?(:x86_64) %}
      File.join(EXAMPLES_BASE_DIR, "bin", "windows-x86_64", "tsanpr.dll")
    {% else %}
      File.join(EXAMPLES_BASE_DIR, "bin", "windows-x86", "tsanpr.dll")
    {% end %}
  {% elsif flag?(:linux) %}
    {% if flag?(:x86_64) %}
      File.join(EXAMPLES_BASE_DIR, "bin", "linux-x86_64", "libtsanpr.so")
    {% elsif flag?(:aarch64) %}
      File.join(EXAMPLES_BASE_DIR, "bin", "linux-aarch64", "libtsanpr.so")
    {% else %}
      ""
    {% end %}
  {% else %}
    ""
  {% end %}
end

def read_image_file(tsanpr : TSANPR, imgfile : String, output_format : String, options : String)
  # Read an image file and call anpr_read_file.
  print "#{imgfile} (outputFormat=\"#{output_format}\", options=\"#{options}\") => "
  result = tsanpr.anpr_read_file(imgfile, output_format, options)
  puts result
end

def read_encoded_image(tsanpr : TSANPR, imgfile : String, output_format : String, options : String)
  # Read an encoded image file as bytes and call tsanpr.anpr_read_pixels with 'encoded' pixel format.
  print "#{imgfile} (outputFormat=\"#{output_format}\", options=\"#{options}\") => "
  
  begin
    unless File.exists?(imgfile)
      puts "File does not exist"
      return
    end
    
    encoded_img = File.read(imgfile)
    result = tsanpr.anpr_read_pixels(
      encoded_img.to_unsafe,
      encoded_img.size.to_u64,
      0_u64,
      0_i64,
      "encoded",
      output_format,
      options
    )
    puts result
  rescue ex
    puts "ERROR: Exception - #{ex.message}"
  end
end

def get_pixel_format(channels : Int32, is_rgb : Bool = false) : String
  # Determine pixel format string based on image channels.
  case channels
  when 1
    "GRAY"
  when 2
    "BGR565"  # or "BGR555"
  when 3
    is_rgb ? "RGB" : "BGR"
  when 4
    is_rgb ? "RGBA" : "BGRA"
  else
    ""
  end
end

def read_pixel_buffer(tsanpr : TSANPR, imgfile : String, output_format : String, options : String)
  # Decode image file and pass pixel buffer to anpr_read_pixels.
  # Falls back to encoded format if decoding fails.
  print "#{imgfile} (outputFormat=\"#{output_format}\", options=\"#{options}\") => "

  begin
    unless File.exists?(imgfile)
      puts "File does not exist"
      return
    end

    # Determine file type by extension
    ext = File.extname(imgfile).downcase

    canvas = case ext
    when ".png"
      StumpyPNG.read(imgfile)
    when ".jpg", ".jpeg"
      # Try to decode JPEG, fallback to encoded format if it fails
      begin
        StumpyJPEG.read(imgfile)
      rescue jpeg_ex
        # StumpyJPEG failed, use encoded format instead
        puts "JPEG decode failed (#{jpeg_ex.message}), using encoded format..."
        encoded_img = File.read(imgfile)
        result = tsanpr.anpr_read_pixels(
          encoded_img.to_unsafe,
          encoded_img.size.to_u64,
          0_u64,
          0_i64,
          "encoded",
          output_format,
          options
        )
        puts result
        return
      end
    else
      puts "Unsupported image format: #{ext}"
      return
    end

    width = canvas.width.to_u64
    height = canvas.height.to_u64
    channels = 3  # RGB format

    # Convert canvas to RGB pixel data
    pixel_data = Bytes.new(width.to_i * height.to_i * channels)

    canvas.height.times do |y|
      canvas.width.times do |x|
        pixel = canvas[x, y]
        offset = (y * canvas.width + x) * channels

        # Extract RGB values (StumpyPNG/JPEG uses UInt16, convert to UInt8)
        pixel_data[offset] = (pixel.r >> 8).to_u8
        pixel_data[offset + 1] = (pixel.g >> 8).to_u8
        pixel_data[offset + 2] = (pixel.b >> 8).to_u8
      end
    end

    # Get pixel format string (RGB format, true = is_rgb)
    pixel_format = get_pixel_format(channels, true)
    if pixel_format.empty?
      puts "Unknown pixel format!"
      return
    end

    # Calculate stride (bytes per row)
    stride = (width * channels).to_i64

    result = tsanpr.anpr_read_pixels(
      pixel_data.to_unsafe,
      width,
      height,
      stride,
      pixel_format,
      output_format,
      options
    )
    puts result
  rescue ex
    puts "ERROR: Exception - #{ex.message}"
  end
end

def read_license_plates(tsanpr : TSANPR, country_code : String)
  # NOTICE:
  # anpr_initialize should be called only once after library load.
  # Therefore, it is not possible to change the country code after anpr_initialize has been called.
  # While using the free trial license, you can try all languages.
  # When you purchase a commercial license, you can only use the selected language.
  error = tsanpr.anpr_initialize("text;country=#{country_code}")
  unless error.empty?
    puts "anpr_initialize() failed: #{error}"
    return
  end
  
  image_dir = File.join(EXAMPLES_BASE_DIR, "img", country_code)
  
  # TODO: Try each function as needed
  anpr_func = ->read_image_file(TSANPR, String, String, String)
  # anpr_func = ->read_encoded_image(TSANPR, String, String, String)
  # anpr_func = ->read_pixel_buffer(TSANPR, String, String, String)
  
  # TODO: Try each output format as needed
  output_format = "text"
  # output_format = "json"
  # output_format = "yaml"
  # output_format = "xml"
  # output_format = "csv"
  
  anpr_func.call(tsanpr, File.join(image_dir, "licensePlate.jpg"), output_format, "")   # Single license plate recognition (default)
  anpr_func.call(tsanpr, File.join(image_dir, "multiple.jpg"), output_format, "vm")     # Recognize multiple license plates attached to vehicles
  anpr_func.call(tsanpr, File.join(image_dir, "multiple.jpg"), output_format, "vmb")    # Recognize multiple license plates attached to vehicles (including motorcycles)
  anpr_func.call(tsanpr, File.join(image_dir, "surround.jpg"), output_format, "vms")    # Recognize multiple license plates attached to vehicles with surround detection
  anpr_func.call(tsanpr, File.join(image_dir, "surround.jpg"), output_format, "dms")    # Recognize multiple surrounding objects (vehicles)
  anpr_func.call(tsanpr, File.join(image_dir, "surround.jpg"), output_format, "dmsr")   # Recognize multiple surrounding objects (vehicles) and license plates
  
  # Recognize multiple surrounding objects and license plates within RoI
  anpr_func.call(tsanpr, File.join(image_dir, "surround.jpg"), output_format, "dmsri549,700,549,2427,1289,2427,1289,700")
end

def main
  engine_file_name = get_engine_file_name
  if engine_file_name.empty? || !File.exists?(engine_file_name)
    puts "Unsupported operating system or engine not found"
    return
  end
  
  begin
    tsanpr = TSANPR.new(engine_file_name)
    
    # TODO: Try each country code as needed
    read_license_plates(tsanpr, "KR")
    # read_license_plates(tsanpr, "JP")
    # read_license_plates(tsanpr, "VN")
    
  rescue ex
    puts "TSANPR initialization failed: #{ex.message}"
  end
end

main