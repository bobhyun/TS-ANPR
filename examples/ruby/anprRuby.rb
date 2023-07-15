#!/usr/bin/ruby -w

#
#  이 예제는 TS-ANPR 엔진 파일을 다운로드받아 examples/bin/ 디렉토리에 
#  압축을 풀어 아래와 같은 디렉토리 구조로 만들어진 상태에서 동작합니다.
#
#  examples
#    /bin
#      /windows-x86_64
#      /windows-x86
#      /linux-x86_64
#      /linux-aarch64
#
#  Dependency install:
#   Debina/Ubuntu:
#     sudo apt-get install build-essential ruby-dev libvips
#   Centos/RHEL:
#     sudo yum groupinstall 'Development Tools'
#
#   sudo gem install ffi ruby-vips


require 'ffi'
require 'pathname'
require 'vips'
require 'rbconfig'

IMG_PATH = '../img/'


def getLibPath ()
  os_name = RbConfig::CONFIG['host_os']
  arch_name = RbConfig::CONFIG['host_cpu']
  printf("os_name=%s, arch_name=%s\n" % [os_name, arch_name])

  if ['win32', 'mingw32'].include?(os_name)
    if ['x86_64', 'x64'].include?(arch_name)      
      return File.expand_path(
        File.join(
          __dir__,
          '..',
          'bin',
          'windows-x86_64',
          'tsanpr.dll'
        )
      )

    elsif ['ia32', 'x86'].include?(arch_name)
      return File.expand_path(
        File.join(
          __dir__,
          '..',
          'bin',
          'windows-x86',
          'tsanpr.dll'
        )
      )
    end

  elsif os_name.include?('linux')
    if arch_name == 'x86_64'
      return File.expand_path(
        File.join(
          __dir__,
          '..',
          'bin',
          'linux-x86_64',
          'libtsanpr.so'
        )
      )
        
    elsif arch_name == 'arm64'
      return File.expand_path(
        File.join(
          __dir__,
          '..',
          'bin',
          'linux-aarch64',
          'libtsanpr.so'
        )
      )
    end
  end

  puts("Unsupported target platform\n")
  exit(-1)
end

module Anpr
  extend FFI::Library
  ffi_lib Pathname.new(getLibPath).realpath  # use absolute path

  # const char* WINAPI anpr_initialize(const char* outputFormat); // [IN] 라이브러리 동작 방식 설정
  attach_function :initialize, :anpr_initialize, [:string], :string

  # const char* WINAPI anpr_read_file(
  #   const char* imgFileName,      // [IN] 입력 이미지 파일명
  #   const char* outputFormat,     // [IN] 출력 데이터 형식
  #   const char* options);         // [IN] 기능 옵션
  attach_function :read_file, :anpr_read_file, [:string, :string, :string], :string

  # const char* WINAPI anpr_read_pixels(
  #   const unsigned char* pixels,  // [IN] 이미지 픽셀 시작 주소
  #   const unsigned long width,    // [IN] 이미지 가로 픽셀 수
  #   const unsigned long height,   // [IN] 이미지 세로 픽셀 수
  #   const unsigned long stride,   // [IN] 이미지 한 라인의 바이트 수
  #   const char* pixelFormat,      // [IN] 이미지 픽셀 형식 
  #   const char* outputFormat,     // [IN] 출력 데이터 형식
  #   const char* options);         // [IN] 기능 옵션
  attach_function :read_pixels, :anpr_read_pixels, [:pointer, :int, :int, :int, :string, :string, :string], :string
end


def readFile (imgFile, outputFormat, options)
  print('%s outputFormat=\"%s\", options=\"%s\") => ' % [imgFile, outputFormat, options])
  result = Anpr.read_file(imgFile, outputFormat, options)
  puts(result)
end


def get_stride(buffer)
  """Gets the stride of a memory buffer."""
  shape = buffer.shape
  stride = shape[1] * shape[2]
  return stride
end

def readPixels (imgFile, outputFormat, options)
  print('%s outputFormat=\"%s\", options=\"%s\") => ' % [imgFile, outputFormat, options])
  
  img = Vips::Image.new_from_file(imgFile)
  if img.nil?    
    puts("Failed to load the image.\n")
    return
  end

  # 이미지를 메모리로 로드
  img.get_fields

  # 이미지를 디코딩한 메모리 버퍼를 얻기
  buffer = img.write_to_memory

  # 픽셀 버퍼 입력으로 차번 인식
  result = Anpr.read_pixels(buffer, img.width, img.height, 0, 'BGR', outputFormat, options)
  puts(result)
end
  
def anprDemo1 (outputFormat)  
  readFile(IMG_PATH + 'licensePlate.jpg', outputFormat, 'v')
  readFile(IMG_PATH + 'licensePlate.jpg', outputFormat, '')
  readFile(IMG_PATH + 'multiple.jpg', outputFormat, 'vm')
  readFile(IMG_PATH + 'multiple.jpg', outputFormat, '')
  readFile(IMG_PATH + 'surround.jpg', outputFormat, 'vms')
  readFile(IMG_PATH + 'surround.jpg', outputFormat, '')
end

def anprDemo2 (outputFormat)
  readPixels(IMG_PATH + 'licensePlate.jpg', outputFormat, 'v')
  readPixels(IMG_PATH + 'licensePlate.jpg', outputFormat, '')
  readPixels(IMG_PATH + 'multiple.jpg', outputFormat, 'vm')
  readPixels(IMG_PATH + 'multiple.jpg', outputFormat, '')
  readPixels(IMG_PATH + 'surround.jpg', outputFormat, 'vms')
  readPixels(IMG_PATH + 'surround.jpg', outputFormat, '')
end

def main
  
  error = Anpr.initialize('text')
  if error != ''
    printf("%s\n" % error)
    exit(1)
  end
 
  # 이미지 파일을 입력으로 사용하는 예제
  anprDemo1('text')
  anprDemo1('json')
  anprDemo1('yaml')
  anprDemo1('xml')
 
  # 픽셀 버퍼를 입력으로 사용하는 예제
  anprDemo2('text')
  anprDemo2('json')
  anprDemo2('yaml')
  anprDemo2('xml')
end

main