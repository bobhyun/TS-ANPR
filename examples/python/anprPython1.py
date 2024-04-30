"""
  이 예제는 TS-ANPR 엔진 파일을 다운로드받아 examples/bin/ 디렉토리에 
  압축을 풀어 아래와 같은 디렉토리 구조로 만들어진 상태에서 동작합니다.

  examples
    /bin
      /windows-x86_64
      /windows-x86
      /linux-x86_64
      /linux-aarch64

  readPixels()를 사용하는 경우 OpenCV, Pillow 두 가지 예제로 구현함
  pip install pillow numpy
  pip install opencv-python
"""

from ctypes import *
import numpy as np
import cv2
from PIL import Image
import sys, os, platform


def getLibPath():
  os_name = platform.system().lower()
  arch_name = platform.machine().lower()
  print('os_name=%s, arch_name=%s' % (os_name, arch_name))

  if os_name == 'windows':
    if arch_name == 'x86_64' or arch_name == 'amd64':
      return os.path.join('..', 'bin', 'windows-x86_64', 'tsanpr.dll')
    elif arch_name == 'x86':
      return os.path.join('..', 'bin', 'windows-x86', 'tsanpr.dll')
  elif os_name == 'linux':
    if arch_name == 'x86_64':
      return os.path.join('..', 'bin', 'linux-x86_64', 'libtsanpr.so')
    elif arch_name == 'aarch64':
      return os.path.join('..', 'bin', 'linux-aarch64', 'libtsanpr.so')

  print('Unsupported target platform')
  sys.exit(-1)


IMG_PATH = '../img/'
LIB_PATH = getLibPath()
print('LIB_PATH=', LIB_PATH)
lib = cdll.LoadLibrary(LIB_PATH)

"""
const char* WINAPI anpr_initialize(const char* mode); // [IN] 라이브러리 동작 방식 설정
"""
lib.anpr_initialize.argtype = c_char_p
lib.anpr_initialize.restype = c_char_p

"""
const char* WINAPI anpr_read_file(
    const char* imgFileName,      // [IN] 입력 이미지 파일명
    const char* outputFormat,     // [IN] 출력 데이터 형식
    const char* options);         // [IN] 기능 옵션
"""
lib.anpr_read_file.argtypes = (c_char_p, c_char_p, c_char_p)
lib.anpr_read_file.restype = c_char_p

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
lib.anpr_read_pixels.argtypes = (c_char_p, c_int32, c_int32, c_int32, c_char_p, c_char_p, c_char_p)
lib.anpr_read_pixels.restype = c_char_p


def initialize():
  error = lib.anpr_initialize('text')
  return error.decode('utf8') if error else error

def readFile(imgFile, outputFormat, options):
  print('{0} (outputFormat=\"{1}\", options=\"{2}\") => '.format(imgFile, outputFormat, options), end='')
  
  # 이미지 파일명 입력으로 차번 인식
  result = lib.anpr_read_file(imgFile.encode('utf-8'), outputFormat.encode('utf-8'), options.encode('utf-8'))
  print(result.decode('utf8'))

def readPixels(imgFile, outputFormat, options):
  print('{0} (outputFormat=\"{1}\", options=\"{2}\") => '.format(imgFile, outputFormat, options), end='')

  # 이미지 파일을 메모리에 로딩 (using OpenCV)
  #img = cv2.imread(imgFile)
  #height = img.shape[0]
  #width = img.shape[1]
  #result = lib.anpr_read_pixels(bytes(img), width, height, 0, 'BGR'.encode('utf-8'), outputFormat.encode('utf-8'), options.encode('utf-8'))

  # 이미지 파일을 메모리에 로딩 (using Pillow)
  img = np.array(Image.open(imgFile))
  height = img.shape[0]
  width = img.shape[1]

  # 픽셀 버퍼 입력으로 차번 인식
  result = lib.anpr_read_pixels(bytes(img), width, height, 0, 'RGB'.encode('utf-8'), outputFormat.encode('utf-8'), options.encode('utf-8'))

  print(result.decode('utf8'))

def anprDemo1(outputFormat):
  # anpr
  readFile(os.path.join(IMG_PATH, 'licensePlate.jpg'), outputFormat, 'v')
  readFile(os.path.join(IMG_PATH, 'licensePlate.jpg'), outputFormat, '')
  readFile(os.path.join(IMG_PATH, 'multiple.jpg'), outputFormat, 'vm')
  readFile(os.path.join(IMG_PATH, 'multiple.jpg'), outputFormat, '')
  readFile(os.path.join(IMG_PATH, 'surround.jpg'), outputFormat, 'vms')
  readFile(os.path.join(IMG_PATH, 'surround.jpg'), outputFormat, '')
  
  # object detection  
  readFile(os.path.join(IMG_PATH, 'surround.jpg'), outputFormat, 'dms')
  readFile(os.path.join(IMG_PATH, 'surround.jpg'), outputFormat, 'dmsr')

def anprDemo2(outputFormat):
  # anpr
  readPixels(os.path.join(IMG_PATH, 'licensePlate.jpg'), outputFormat, 'v')
  readPixels(os.path.join(IMG_PATH, 'licensePlate.jpg'), outputFormat, '')
  readPixels(os.path.join(IMG_PATH, 'multiple.jpg'), outputFormat, 'vm')
  readPixels(os.path.join(IMG_PATH, 'multiple.jpg'), outputFormat, '')
  readPixels(os.path.join(IMG_PATH, 'surround.jpg'), outputFormat, 'vms')
  readPixels(os.path.join(IMG_PATH, 'surround.jpg'), outputFormat, '')
  
  # object detection  
  readPixels(os.path.join(IMG_PATH, 'surround.jpg'), outputFormat, 'dms')
  readPixels(os.path.join(IMG_PATH, 'surround.jpg'), outputFormat, 'dmsr')

def main():
  error = initialize()
  if error:
    print(error)
    sys.exit(1)

  # 이미지 파일을 입력으로 사용하는 예제
  anprDemo1('text')
  anprDemo1('json')
  anprDemo1('yaml')
  anprDemo1('xml')
  anprDemo1('csv')

  # 픽셀 버퍼를 입력으로 사용하는 예제
  anprDemo2('text')
  anprDemo2('json')
  anprDemo2('yaml')
  anprDemo2('xml')
  anprDemo2('csv')


if __name__ == '__main__':
  main()

