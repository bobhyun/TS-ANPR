using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;


namespace anprCsharp1
{
  internal class Program
  {
    static string IMG_PATH = "..\\..\\..\\..\\img\\";

    [DllImport("..\\..\\tsanpr.dll")]
    static extern IntPtr anpr_initialize(
      [MarshalAs(UnmanagedType.LPUTF8Str)] string outputFormat); // [IN] 오류 발생시 출력 데이터 형식
    [DllImport("..\\..\\tsanpr.dll")]
    static extern IntPtr anpr_read_file(
      [MarshalAs(UnmanagedType.LPUTF8Str)] string imgFileName,    // [IN] 입력 이미지 파일명
      [MarshalAs(UnmanagedType.LPUTF8Str)] string outputFormat,   // [IN] 출력 데이터 형식
      [MarshalAs(UnmanagedType.LPUTF8Str)] string options);       // [IN] 기능 옵션
    [DllImport("..\\..\\tsanpr.dll")]
    static extern IntPtr anpr_read_pixels(
      [MarshalAs(UnmanagedType.LPUTF8Str)] string pixels,         // [IN] 이미지 픽셀 시작 주소
      [MarshalAs(UnmanagedType.LPUTF8Str)] string width,          // [IN] 이미지 가로 픽셀 수
      [MarshalAs(UnmanagedType.LPUTF8Str)] string height,         // [IN] 이미지 세로 픽셀 수
      [MarshalAs(UnmanagedType.LPUTF8Str)] string stride,         // [IN] 이미지 한 라인의 바이트 수
      [MarshalAs(UnmanagedType.LPUTF8Str)] string pixelFormat,    // [IN] 이미지 픽셀 형식
      [MarshalAs(UnmanagedType.LPUTF8Str)] string outputFormat,   // [IN] 출력 데이터 형식
      [MarshalAs(UnmanagedType.LPUTF8Str)] string options);       // [IN] 기능 옵션

    static string ptrToUtf8(IntPtr nativeUtf8)
    {
      int len = 0;
      while (Marshal.ReadByte(nativeUtf8, len) != 0) ++len;
      byte[] buffer = new byte[len];
      Marshal.Copy(nativeUtf8, buffer, 0, buffer.Length);
      string str = Encoding.UTF8.GetString(buffer);
      buffer = null;
      return str;
    }

    static void readFile(string imgfile, string outputFormat, string options)
    {
      Console.Write("{0} (outputFormat=\"{1}\", options=\"{2}\") => ", imgfile, outputFormat, options);
      IntPtr result = anpr_read_file(imgfile, outputFormat, options);
      Console.WriteLine(ptrToUtf8(result));
    }

    static void anprDemo1(string outputFormat)
    {
      readFile(IMG_PATH + "licensePlate.jpg", outputFormat, "v");
      readFile(IMG_PATH + "licensePlate.jpg", outputFormat, "");
      readFile(IMG_PATH + "multiple.jpg", outputFormat, "vm");
      readFile(IMG_PATH + "multiple.jpg", outputFormat, "");
      readFile(IMG_PATH + "surround.jpg", outputFormat, "vms");
      readFile(IMG_PATH + "surround.jpg", outputFormat, "");
    }

  static void Main(string[] args)
    {
      Console.OutputEncoding = System.Text.Encoding.UTF8;

      IntPtr error = anpr_initialize("text");
      if (error != IntPtr.Zero)
      {
        Console.WriteLine(ptrToUtf8(error));
        return;
      }

      anprDemo1("text");
      anprDemo1("json");
      anprDemo1("yaml");
      anprDemo1("xml");
      anprDemo1("csv");
    }
  }
}
