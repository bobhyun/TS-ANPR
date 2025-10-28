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

import std.string;
import std.file;

version(Windows)
{
    import core.sys.windows.windows;
}
else
{
    import core.sys.posix.dlfcn;
}

version(Windows)
{
    alias LibHandle = HMODULE;
    alias getFuncAddress = GetProcAddress;
    LibHandle loadLibrary(string path) { return LoadLibraryA(toStringz(path)); }
    void unloadLibrary(LibHandle handle) { FreeLibrary(handle); }
}
else
{
    alias LibHandle = void*;
    alias getFuncAddress = dlsym;
    LibHandle loadLibrary(string path) { return dlopen(toStringz(path), RTLD_LAZY); }
    void unloadLibrary(LibHandle handle) { dlclose(handle); }
}

extern(C) alias AnprInitializeFunc = const(char)* function(const(char)* mode);
extern(C) alias AnprReadFileFunc = const(char)* function(const(char)* imgFileName, const(char)* outputFormat, const(char)* options);
extern(C) alias AnprReadPixelsFunc = const(char)* function(const(void)* pixels, ulong width, ulong height, long stride, const(char)* pixelFormat, const(char)* outputFormat, const(char)* options);

class TSANPR
{
    private LibHandle libHandle;
    private AnprInitializeFunc anprInitializeFunc;
    private AnprReadFileFunc anprReadFileFunc;
    private AnprReadPixelsFunc anprReadPixelsFunc;

    this(string libraryPath)
    {
        if (!exists(libraryPath))
            throw new Exception("Library file not found: " ~ libraryPath);

        libHandle = loadLibrary(libraryPath);
        if (!libHandle)
            throw new Exception("Failed to load native library: " ~ libraryPath);

        anprInitializeFunc = cast(AnprInitializeFunc)getFuncAddress(libHandle, "anpr_initialize");
        anprReadFileFunc = cast(AnprReadFileFunc)getFuncAddress(libHandle, "anpr_read_file");
        anprReadPixelsFunc = cast(AnprReadPixelsFunc)getFuncAddress(libHandle, "anpr_read_pixels");

        if (!anprInitializeFunc || !anprReadFileFunc || !anprReadPixelsFunc)
            throw new Exception("Failed to load required functions from library");
    }

    ~this()
    {
        if (libHandle)
            unloadLibrary(libHandle);
    }

    string anprInitialize(string mode)
    {
        try
        {
            auto result = anprInitializeFunc(toStringz(mode));
            return result ? fromStringz(result).idup : "";
        }
        catch (Exception e)
        {
            return "Function call failed: " ~ e.msg;
        }
    }

    string anprReadFile(string imgFileName, string outputFormat, string options)
    {
        try
        {
            auto result = anprReadFileFunc(toStringz(imgFileName), toStringz(outputFormat), toStringz(options));
            return result ? fromStringz(result).idup : "";
        }
        catch (Exception e)
        {
            return "Function call failed: " ~ e.msg;
        }
    }

    string anprReadPixels(const(void)* pixels, size_t width, size_t height, long stride, string pixelFormat, string outputFormat, string options)
    {
        try
        {
            auto result = anprReadPixelsFunc(pixels, width, height, stride, toStringz(pixelFormat), toStringz(outputFormat), toStringz(options));
            return result ? fromStringz(result).idup : "";
        }
        catch (Exception e)
        {
            return "Function call failed: " ~ e.msg;
        }
    }
}