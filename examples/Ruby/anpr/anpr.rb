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

require 'rbconfig'
require 'fileutils'
require 'ffi'
require 'vips'

# Set base directory
EXAMPLES_BASE_DIR = File.expand_path(File.join(__dir__, "../.."))

# Determine native library path based on OS and architecture
def get_engine_file_name
  arch = RbConfig::CONFIG['host_cpu']
  os = RbConfig::CONFIG['host_os']

  if os =~ /mswin|mingw|cygwin/
    if arch =~ /x64|amd64|x86_64/
      return File.join(EXAMPLES_BASE_DIR, 'bin/windows-x86_64/tsanpr.dll')
    elsif arch =~ /x86|i386/
      return File.join(EXAMPLES_BASE_DIR, 'bin/windows-x86/tsanpr.dll')
    end
  elsif os =~ /linux/
    if arch =~ /x86_64|amd64/
      return File.join(EXAMPLES_BASE_DIR, 'bin/linux-x86_64/libtsanpr.so')
    elsif arch =~ /aarch64/
      return File.join(EXAMPLES_BASE_DIR, 'bin/linux-aarch64/libtsanpr.so')
    end
  end
  ""
end

# Set native library path as environment variable
engine_file_name = get_engine_file_name
if engine_file_name.empty? || !File.exist?(engine_file_name)
  puts "Unsupported operating system or engine not found"
  exit(1)
end
ENV['TSANPR_LIB_PATH'] = engine_file_name

require_relative './lib/tsanpr'

# call anpr_read_file with image file name
def read_image_file(tsanpr, imgfile, output_format, options)
  print "#{imgfile} (outputFormat=\"#{output_format}\", options=\"#{options}\") => "
  result = tsanpr.anpr_read_file(imgfile, output_format, options)
  puts result
end

# Read an encoded image file (e.g., JPG/PNG) as a binary buffer and call anpr_read_pixels with "encoded" pixel format
def read_encoded_image(tsanpr, imgfile, output_format, options)
  print "#{imgfile} (outputFormat=\"#{output_format}\", options=\"#{options}\") => "
  begin
    encoded_img = File.binread(imgfile)
    # Create an FFI::MemoryPointer from the binary data
    FFI::MemoryPointer.new(:uint8, encoded_img.bytesize) do |buf|
      buf.put_bytes(0, encoded_img)
      # Call the wrapper function defined in tsanpr.rb
      result = tsanpr.anpr_read_pixels(
        buf, encoded_img.bytesize, 0, 0, "encoded", output_format, options
      )
      puts result
    end
  rescue => e
    puts "\nERROR: Exception - #{e.message}"
  end
end

# Read image file into pixel buffer and call anpr_read_pixels
def read_pixel_buffer(tsanpr, imgfile, output_format, options)
  print "#{imgfile} (outputFormat=\"#{output_format}\", options=\"#{options}\") => "
  begin
    image = Vips::Image.new_from_file(imgfile, access: :sequential)
    width = image.width
    height = image.height
    bands = image.bands # 3 for RGB, 4 for RGBA, 1 for GRAY, etc.

    # Determine pixel format string based on bands
    pixel_format =
      case bands
      when 1
        "GRAY"
      when 3
        "RGB"
      when 4
        "RGBA"
      else
        raise "Unsupported number of bands: #{bands}"
      end

    # Get raw pixel buffer (binary string)
    pixels = image.write_to_memory
    stride = width * bands

    # If BGR is needed, swap channels here
    FFI::MemoryPointer.new(:uint8, pixels.bytesize) do |buf|
      buf.put_bytes(0, pixels)
      result = tsanpr.anpr_read_pixels(buf, width, height, stride, pixel_format, output_format, options)
      puts result
    end
  rescue => e
    puts "\nERROR: Exception - #{e.message}"
  end
end

# Run license plate recognition for a country
def read_license_plates(tsanpr, country_code)
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

  # TODO:  Try each function as needed
  read_func = method(:read_image_file)
  #read_func = method(:read_encoded_image)
  # read_func = method(:read_pixel_buffer)

  # TODO: Try each output format as needed
  output_format = "text"
  # output_format = "json"
  # output_format = "yaml"
  # output_format = "xml"
  # output_format = "csv"

  read_func.call(tsanpr, File.join(image_dir, "licensePlate.jpg"), output_format, "") # Single license plate recognition (default)
  read_func.call(tsanpr, File.join(image_dir, "multiple.jpg"), output_format, "vm")   # Recognize multiple license plates attached to vehicles
  read_func.call(tsanpr, File.join(image_dir, "multiple.jpg"), output_format, "vmb")  # Recognize multiple license plates attached to vehicles (including motorcycles)
  read_func.call(tsanpr, File.join(image_dir, "surround.jpg"), output_format, "vms")  # Recognize multiple license plates attached to vehicles with surround detection
  read_func.call(tsanpr, File.join(image_dir, "surround.jpg"), output_format, "dms")  # Recognize multiple surrounding objects (vehicles)
  read_func.call(tsanpr, File.join(image_dir, "surround.jpg"), output_format, "dmsr") # Recognize multiple surrounding objects (vehicles) and license plates
end

def main
  tsanpr = TSANPR.new

  # TODO: Try each country code as needed
  read_license_plates(tsanpr, "KR")
  # read_license_plates(tsanpr, "JP")
  # read_license_plates(tsanpr, "VN")
end

main if __FILE__ == $0
