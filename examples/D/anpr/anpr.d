/*
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
*/

import std.stdio;
import std.file;
import std.path;
import std.string;
import tsanpr;
import imageformats;

string getExamplesBaseDir()
{
    return buildPath(dirName(dirName(getcwd())));
}

string getEngineFileName()
{
    string baseDir = getExamplesBaseDir();
    version(Windows)
    {
        version(X86_64)
            return buildPath(baseDir, "bin", "windows-x86_64", "tsanpr.dll");
        else
            return buildPath(baseDir, "bin", "windows-x86", "tsanpr.dll");
    }
    else version(linux)
    {
        version(X86_64)
            return buildPath(baseDir, "bin", "linux-x86_64", "libtsanpr.so");
        else version(AArch64)
            return buildPath(baseDir, "bin", "linux-aarch64", "libtsanpr.so");
        else
            return "";
    }
    else
        return "";
}

void readImageFile(TSANPR tsanpr, string imgfile, string outputFormat, string options)
{
    writef("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options);
    string result = tsanpr.anprReadFile(imgfile, outputFormat, options);
    writeln(result);
}

void readEncodedImage(TSANPR tsanpr, string imgfile, string outputFormat, string options)
{
    writef("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options);
    
    try
    {
        if (!exists(imgfile))
        {
            writeln("File does not exist");
            return;
        }
        
        ubyte[] encodedImg = cast(ubyte[])read(imgfile);
        string result = tsanpr.anprReadPixels(
            encodedImg.ptr,
            encodedImg.length,
            0,
            0,
            "encoded",
            outputFormat,
            options
        );
        writeln(result);
    }
    catch (Exception e)
    {
        writefln("ERROR: Exception - %s", e.msg);
    }
}

string getPixelFormat(int channels, bool isRGB = false)
{
    switch (channels)
    {
        case 1: return "GRAY";
        case 2: return "BGR565";
        case 3: return isRGB ? "RGB" : "BGR";
        case 4: return isRGB ? "RGBA" : "BGRA";
        default: return "";
    }
}

void readPixelBuffer(TSANPR tsanpr, string imgfile, string outputFormat, string options)
{
    writef("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options);

    try
    {
        if (!exists(imgfile))
        {
            writeln("File does not exist");
            return;
        }

        // Read and decode image using imageformats library
        IFImage img = read_image(imgfile, ColFmt.RGB);

        if (img.w == 0 || img.h == 0)
        {
            writeln("Image load failed!");
            return;
        }

        // Get pixel format (imageformats returns RGB/RGBA format)
        string pixelFormat = getPixelFormat(img.c, true);
        if (pixelFormat.length == 0)
        {
            writeln("Unknown pixel format!");
            return;
        }

        long stride = img.w * img.c;
        string result = tsanpr.anprReadPixels(
            img.pixels.ptr,
            img.w,
            img.h,
            stride,
            pixelFormat,
            outputFormat,
            options
        );
        writeln(result);
    }
    catch (Exception e)
    {
        writefln("ERROR: Exception - %s", e.msg);
    }
}

void readLicensePlates(TSANPR tsanpr, string countryCode)
{
    string initParams = "text;country=" ~ countryCode;
    string error = tsanpr.anprInitialize(initParams);
    if (error.length > 0)
    {
        writefln("anpr_initialize() failed: %s", error);
        return;
    }

    string imageDir = buildPath(getExamplesBaseDir(), "img", countryCode);
    
    // TODO: Try each function as needed
    auto anprFunc = &readImageFile;
    // auto anprFunc = &readEncodedImage;
    // auto anprFunc = &readPixelBuffer;
    
    // TODO: Try each output format as needed
    string outputFormat = "text";
    // string outputFormat = "json";
    // string outputFormat = "yaml";
    // string outputFormat = "xml";
    // string outputFormat = "csv";
    
    anprFunc(tsanpr, buildPath(imageDir, "licensePlate.jpg"), outputFormat, "");   // Single license plate recognition (default)
    anprFunc(tsanpr, buildPath(imageDir, "multiple.jpg"), outputFormat, "vm");     // Recognize multiple license plates attached to vehicles
    anprFunc(tsanpr, buildPath(imageDir, "multiple.jpg"), outputFormat, "vmb");    // Recognize multiple license plates attached to vehicles (including motorcycles)
    anprFunc(tsanpr, buildPath(imageDir, "surround.jpg"), outputFormat, "vms");    // Recognize multiple license plates attached to vehicles with surround detection
    anprFunc(tsanpr, buildPath(imageDir, "surround.jpg"), outputFormat, "dms");    // Recognize multiple surrounding objects (vehicles)
    anprFunc(tsanpr, buildPath(imageDir, "surround.jpg"), outputFormat, "dmsr");   // Recognize multiple surrounding objects (vehicles) and license plates
    
    // Recognize multiple surrounding objects and license plates within RoI
    anprFunc(tsanpr, buildPath(imageDir, "surround.jpg"), outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700");
}

void main()
{
    string engineFileName = getEngineFileName();
    if (engineFileName.length == 0 || !exists(engineFileName))
    {
        writeln("Unsupported operating system or engine not found");
        return;
    }
    
    try
    {
        auto tsanpr = new TSANPR(engineFileName);
        
        // TODO: Try each country code as needed
        readLicensePlates(tsanpr, "KR");
        // readLicensePlates(tsanpr, "JP");
        // readLicensePlates(tsanpr, "VN");
    }
    catch (Exception e)
    {
        writefln("TSANPR initialization failed: %s", e.msg);
    }
}