using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using static System.Net.Mime.MediaTypeNames;
using System.Drawing;


namespace anprCsharpDotnet1
{
  internal class Program
  {
    static string IMG_PATH = "..\\..\\img\\";

    [DllImport("tsanpr.dll")]
    static extern IntPtr anpr_initialize(
      [MarshalAs(UnmanagedType.LPUTF8Str)] string outputFormat); // [IN] 오류 발생시 출력 데이터 형식
    [DllImport("tsanpr.dll")]
    static extern IntPtr anpr_read_file(
      [MarshalAs(UnmanagedType.LPUTF8Str)] string imgFileName,    // [IN] 입력 이미지 파일명
      [MarshalAs(UnmanagedType.LPUTF8Str)] string outputFormat,   // [IN] 출력 데이터 형식
      [MarshalAs(UnmanagedType.LPUTF8Str)] string options);       // [IN] 기능 옵션
    [DllImport("tsanpr.dll")]
    static extern IntPtr anpr_read_pixels(
      IntPtr pixels,                                              // [IN] 이미지 픽셀 시작 주소
      int width,                                                  // [IN] 이미지 가로 픽셀 수
      int height,                                                 // [IN] 이미지 세로 픽셀 수
      int stride,                                                 // [IN] 이미지 한 라인의 바이트 수
      [MarshalAs(UnmanagedType.LPUTF8Str)] string pixelFormat,    // [IN] 이미지 픽셀 형식
      [MarshalAs(UnmanagedType.LPUTF8Str)] string outputFormat,   // [IN] 출력 데이터 형식
      [MarshalAs(UnmanagedType.LPUTF8Str)] string options);       // [IN] 기능 옵션

    static string ptrToUtf8(IntPtr nativeUtf8)
    {
      if (nativeUtf8 == IntPtr.Zero)
        return "";
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
    
    static void readPixels(string imgfile, string outputFormat, string options)
    {
      Console.Write("{0} (outputFormat=\"{1}\", options=\"{2}\") => ", imgfile, outputFormat, options);
      
      Bitmap bmp = new Bitmap(imgfile);
      Rectangle rect = new Rectangle(0, 0, bmp.Width, bmp.Height);
      System.Drawing.Imaging.BitmapData bmpData =
        bmp.LockBits(rect, System.Drawing.Imaging.ImageLockMode.ReadWrite, bmp.PixelFormat);

      IntPtr result = anpr_read_pixels(bmpData.Scan0, bmpData.Width, bmpData.Height, bmpData.Stride, "BGR", outputFormat, options);
      Console.WriteLine(ptrToUtf8(result));

      // Fixed: 2022.10.28. Bob Hyun.
      // Don't forget to release bitmap memory
      bmp.UnlockBits(bmpData);
      bmp.Dispose();
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
    static void anprDemo2(string outputFormat)
    {
      readPixels(IMG_PATH + "licensePlate.jpg", outputFormat, "v");
      readPixels(IMG_PATH + "licensePlate.jpg", outputFormat, "");
      readPixels(IMG_PATH + "multiple.jpg", outputFormat, "vm");
      readPixels(IMG_PATH + "multiple.jpg", outputFormat, "");
      readPixels(IMG_PATH + "surround.jpg", outputFormat, "vms");
      readPixels(IMG_PATH + "surround.jpg", outputFormat, "");
    }

    static void Main(string[] args)
    {
      Console.OutputEncoding = System.Text.Encoding.UTF8;

      IntPtr ptr = anpr_initialize("text");
      string error = ptrToUtf8(ptr);
      if (error != "")
      {
        Console.WriteLine(error);
        return;
      }

      anprDemo1("text");
      anprDemo1("json");
      anprDemo1("yaml");
      anprDemo1("xml");
      anprDemo1("csv");

      anprDemo2("text");
      anprDemo2("json");
      anprDemo2("yaml");
      anprDemo2("xml");
      anprDemo2("csv");
    }
  }
}
