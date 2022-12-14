# readPixels()를 사용하는 경우 OpenCV, Pillow 두 가지 예제로 구현함
# pip install pillow
# pip install opencv-python

from ctypes import *
import numpy as np
import cv2
from PIL import Image
import sys

sys.stdout.reconfigure(encoding='utf-8')

IMG_PATH = '..\\img\\'
dll = cdll.LoadLibrary('..\\bin\\x64\\tsanpr.dll')

"""
const char* WINAPI anpr_initialize(const char* outputFormat); // [IN] 오류 발생시 출력 데이터 형식
"""
dll.anpr_initialize.argtype = c_char_p
dll.anpr_initialize.restype = c_char_p

"""
const char* WINAPI anpr_read_file(
    const char* imgFileName,      // [IN] 입력 이미지 파일명
    const char* outputFormat,     // [IN] 출력 데이터 형식
    const char* options);         // [IN] 기능 옵션
"""
dll.anpr_read_file.argtypes = (c_char_p, c_char_p, c_char_p)
dll.anpr_read_file.restype = c_char_p

"""
const char* WINAPI anpr_read_pixels(
  const unsigned char* pixels,  // [IN] 이미지 픽셀 시작 주소
  const unsigned long width,    // [IN] 이미지 가로 픽셀 수
  const unsigned long height,   // [IN] 이미지 세로 픽셀 수
  const unsigned long stride,   // [IN] 이미지 한 라인의 바이트 수
  const char* pixelFormat,      // [IN] 이미지 픽셀 형식 
  const char* outputFormat,     // [IN] 출력 데이터 형식
  const char* options);         // [IN] 기능 옵션
"""
dll.anpr_read_pixels.argtypes = (c_char_p, c_int32, c_int32, c_int32, c_char_p, c_char_p, c_char_p)
dll.anpr_read_pixels.restype = c_char_p


def initialize():
  error = dll.anpr_initialize('text')
  return error.decode('utf8') if error else error

def readFile(imgFile, outputFormat, options):
  print('{0} (outputFormat=\"{1}\", options=\"{2}\") => '.format(imgFile, outputFormat, options), end='')
  result = dll.anpr_read_file(imgFile.encode('utf-8'), outputFormat.encode('utf-8'), options.encode('utf-8'))
  print(result.decode('utf8'))

def readPixels(imgFile, outputFormat, options):
  print('{0} (outputFormat=\"{1}\", options=\"{2}\") => '.format(imgFile, outputFormat, options), end='')

  # using OpenCV
  #img = cv2.imread(imgFile)
  #height = img.shape[0]
  #width = img.shape[1]
  #result = dll.anpr_read_pixels(bytes(img), width, height, 0, 'BGR'.encode('utf-8'), outputFormat.encode('utf-8'), options.encode('utf-8'))

  # using Pillow
  img = np.array(Image.open(imgFile))
  height = img.shape[0]
  width = img.shape[1]
  result = dll.anpr_read_pixels(bytes(img), width, height, 0, 'RGB'.encode('utf-8'), outputFormat.encode('utf-8'), options.encode('utf-8'))

  print(result.decode('utf8'))

def anprDemo1(outputFormat):
  readFile(IMG_PATH + 'licensePlate.jpg', outputFormat, 'v')
  readFile(IMG_PATH + 'licensePlate.jpg', outputFormat, '')
  readFile(IMG_PATH + 'multiple.jpg', outputFormat, 'vm')
  readFile(IMG_PATH + 'multiple.jpg', outputFormat, '')
  readFile(IMG_PATH + 'surround.jpg', outputFormat, 'vms')
  readFile(IMG_PATH + 'surround.jpg', outputFormat, '')

def anprDemo2(outputFormat):
  readPixels(IMG_PATH + 'licensePlate.jpg', outputFormat, 'v')
  readPixels(IMG_PATH + 'licensePlate.jpg', outputFormat, '')
  readPixels(IMG_PATH + 'multiple.jpg', outputFormat, 'vm')
  readPixels(IMG_PATH + 'multiple.jpg', outputFormat, '')
  readPixels(IMG_PATH + 'surround.jpg', outputFormat, 'vms')
  readPixels(IMG_PATH + 'surround.jpg', outputFormat, '')

def main():
  error = initialize()
  if error:
    print(error)
    exit(1)

  anprDemo1('text')
  anprDemo1('json')
  anprDemo1('yaml')
  anprDemo1('xml')
  anprDemo1('csv')

  anprDemo2('text')
  anprDemo2('json')
  anprDemo2('yaml')
  anprDemo2('xml')
  anprDemo2('csv')


if __name__ == '__main__':
  main()

