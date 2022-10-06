#!/usr/bin/ruby -w

#gem install ffi ruby-vips


require 'ffi'
require 'pathname'
require 'vips'

IMG_PATH = '../img/'

module Anpr
  extend FFI::Library
  ffi_lib Pathname.new('../bin/x64/tsanpr.dll').realpath  # use absolute path

  # const char* WINAPI anpr_initialize(const char* outputFormat); // [IN] 오류 발생시 출력 데이터 형식
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
  attach_function :read_pixels, :anpr_read_pixels, [:string, :int, :int, :int, :string, :string, :string], :string
end


def readFile (imgFile, outputFormat, options)
  print '%s outputFormat=\"%s\", options=\"%s\") => ' % [imgFile, outputFormat, options]
  result = Anpr.read_file imgFile, outputFormat, options
  puts result
end

def readPixels (imgFile, outputFormat, options)
  print '%s outputFormat=\"%s\", options=\"%s\") => ' % [imgFile, outputFormat, options]
  
  img = Vips::Image.new_from_file imgFile
  
  # [알림]
  # 제가 Ruby 프로그래밍에 익숙하지 못해
  # 로딩된 이미지의 픽셀 버퍼 주소를 직접 얻는 방법을 모르겠네요.
  # 아래 주석 처리한 Anpr.read_pixels(img.buffer, ...) 부분을 마무리하지 못했습니다.
  #
  # 만약 방법을 알고 계시면
  # 아래 저희 프로젝트 게시판에 글을 남겨주시면 고맙겠습니다. ^^;
  # https://github.com/bobhyun/TS-ANPR/issues/new

  #result = Anpr.read_pixels(img.buffer, img.width, img.height, 0, 'BGR', outputFormat, options);
  #puts result
end
  
def anprDemo1 (outputFormat)
  return
  
  readFile IMG_PATH + 'licensePlate.jpg', outputFormat, 'v'
  readFile IMG_PATH + 'licensePlate.jpg', outputFormat, ''
  readFile IMG_PATH + 'multiple.jpg', outputFormat, 'vm'
  readFile IMG_PATH + 'multiple.jpg', outputFormat, ''
  readFile IMG_PATH + 'surround.jpg', outputFormat, 'vms'
  readFile IMG_PATH + 'surround.jpg', outputFormat, ''
end

def anprDemo2 (outputFormat)
  readPixels IMG_PATH + 'licensePlate.jpg', outputFormat, 'v'
  readPixels IMG_PATH + 'licensePlate.jpg', outputFormat, ''
  readPixels IMG_PATH + 'multiple.jpg', outputFormat, 'vm'
  readPixels IMG_PATH + 'multiple.jpg', outputFormat, ''
  readPixels IMG_PATH + 'surround.jpg', outputFormat, 'vms'
  readPixels IMG_PATH + 'surround.jpg', outputFormat, ''
end

def main
  
  error = Anpr.initialize 'text'
  if error != ''
    print error
    exit 1
  end
 
  anprDemo1 'text'
  anprDemo1 'json'
  anprDemo1 'yaml'
  anprDemo1 'xml'
  anprDemo1 'csv'
 
  anprDemo2 'text'
  anprDemo2 'json'
  anprDemo2 'yaml'
  anprDemo2 'xml'
  anprDemo2 'csv'
end

main