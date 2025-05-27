/**
* The MIT License (MIT)
* Copyright © 2022-2025 TS-Solution Corp.
* 
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to all conditions.
* 
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
* 
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
**/

using System;
using System.Runtime.InteropServices;
using OpenCvSharp;
using static System.Runtime.InteropServices.JavaScript.JSType;

class Program
{
    private static TSANPR? tsanpr;

    // Select the engine file path based on OS and process bitness
    static readonly string examplesBaseDir = "../../../../../..";

    // Determine the engine file path according to OS and architecture
    static string GetEngineFileName()
    {
        Architecture arch = RuntimeInformation.ProcessArchitecture;
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            switch (arch)
            {
                case Architecture.X86:
                    return examplesBaseDir + "/bin/windows-x86/tsanpr.dll";
                case Architecture.X64:
                    return examplesBaseDir + "/bin/windows-x86_64/tsanpr.dll";
            }
        }
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
        {
            switch (arch)
            {
                case Architecture.X64:
                    return examplesBaseDir + "/bin/linux-x86_64/libtsanpr.so";
                case Architecture.Arm64:
                    return examplesBaseDir + "/bin/linux-aarch64/libtsanpr.so";
            }
        }

        return "";
    }

    public delegate void AnprReadDelegate(string imgfile, string outputFormat, string options);

    static void ReadImageFile(string imgfile, string outputFormat, string options)
    {
        if (tsanpr == null) return;

        Console.Write($"{imgfile} (outputFormat=\"{outputFormat}\", options=\"{options}\") => ");
        string result = tsanpr.anpr_read_file(imgfile, outputFormat, options);
        Console.WriteLine(result);
    }

    static void ReadEncodedImage(string imgfile, string outputFormat, string options)
    {
        if (tsanpr == null) return;

        Console.Write($"{imgfile} (outputFormat=\"{outputFormat}\", options=\"{options}\") => ");

        try
        {
            byte[] encodedImg = File.ReadAllBytes(imgfile);
            GCHandle handle = GCHandle.Alloc(encodedImg, GCHandleType.Pinned);
            try
            {
                IntPtr ptr = handle.AddrOfPinnedObject();
                string result = tsanpr.anpr_read_pixels(
                    ptr,
                    (ulong)encodedImg.Length,
                    0,
                    0,
                    "encoded",
                    outputFormat,
                    options
                );
                Console.WriteLine(result);
            }
            finally
            {
                handle.Free();
            }
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"\nERROR: Exception - {ex.Message}");
        }
    }

    // Helper: Determine pixel format string based on OpenCvSharp Mat channels
    static string? GetPixelFormat(Mat img)
    {
        int channels = img.Channels();
        if (channels == 1)
            return "GRAY";
        else if (channels == 2)
            return "BGR565"; // or "BGR555"
        else if (channels == 3)
            return "BGR";
        else if (channels == 4)
            return "BGRA";
        else
            return null;
    }

    // Use the pixel buffer-based ANPR function
    static void ReadPixelBuffer(string imgfile, string outputFormat, string options)
    {
        if (tsanpr == null) return;

        Console.Write($"{imgfile} (outputFormat=\"{outputFormat}\", options=\"{options}\") => ");

        using var img = Cv2.ImRead(imgfile, ImreadModes.Unchanged);
        if (img.Empty())
        {
            Console.Error.WriteLine("Image load failed!");
            return;
        }

        string? pixelFormat = GetPixelFormat(img);
        if (pixelFormat == null)
        {
            Console.Error.WriteLine("Unknown pixel format!");
            return;
        }

        IntPtr imgData = img.Data;
        int stride = (int)img.Step();

        string result = tsanpr.anpr_read_pixels(
            imgData,
            (ulong)img.Width,
            (ulong)img.Height,
            stride,
            pixelFormat,
            outputFormat,
            options
        );
        Console.WriteLine(result);
    }

    static void readLicensePlates(string countryCode)
    {
        if (tsanpr == null) return;

        /**
         * NOTICE:
         * anpr_initialize should be called only once after library load.
         * Therefore, it is not possible to change the country code after anpr_initialize has been called.
         * While using the free trial license, you can try all languages.
         * When you purchase a commercial license, you can only use the selected language.
         */
        string error = tsanpr.anpr_initialize("text;country=" + countryCode);
        if (!string.IsNullOrEmpty(error))
        {
            Console.WriteLine($"anpr_initialize() failed: {error}");
            return;
        }

        string imageDir = examplesBaseDir + "/img/" + countryCode + "/";

        // TODO: Try each function as needed
        AnprReadDelegate anprDelegate = ReadImageFile;
        //AnprReadDelegate anprDelegate = ReadEncodedImage;
        //AnprReadDelegate anprDelegate = ReadPixelBuffer;

        // TODO: Try each output format as needed
        const string outputFormat = "text";
        // const string outputFormat = "json"
        // const string outputFormat = "yaml"
        // const string outputFormat = "xml"
        // const string outputFormat = "csv"
        
        anprDelegate(imageDir + "licensePlate.jpg", outputFormat, "");  // Single license plate recognition (default)
        anprDelegate(imageDir + "multiple.jpg", outputFormat, "vm");    // Recognize multiple license plates attached to vehicles
        anprDelegate(imageDir + "multiple.jpg", outputFormat, "vmb");   // Recognize multiple license plates attached to vehicles (including motorcycles)
        anprDelegate(imageDir + "surround.jpg", outputFormat, "vms");   // Recognize multiple license plates attached to vehicles with surround detection
        anprDelegate(imageDir + "surround.jpg", outputFormat, "dms");   // Recognize multiple surrounding objects (vehicles)
        anprDelegate(imageDir + "surround.jpg", outputFormat, "dmsr");  // Recognize multiple surrounding objects (vehicles) and license plates
        
        // Recognize multiple surrounding objects and license plates within RoI
        anprDelegate(imageDir + "surround.jpg", outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700");
    }

    static void Main(string[] args)
    {
        // Set console output to UTF-8
        Console.OutputEncoding = System.Text.Encoding.UTF8;

        string engineFileName = GetEngineFileName();
        if (string.IsNullOrEmpty(engineFileName))
        {
            Console.WriteLine("Unsupported operating system");
            return;
        }

        try
        {
            tsanpr = new TSANPR(engineFileName);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"TSANPR initialization failed: {ex.Message}");
            return;
        }

        // TODO: Try each country code as needed
        readLicensePlates("KR");
        // readLicensePlates("JP");
        // readLicensePlates("VN");
    }
}
