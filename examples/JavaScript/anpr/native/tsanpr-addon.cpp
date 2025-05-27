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

#include <napi.h>
#include <string>
#include "tsanpr.h"

static TSANPR tsanpr;

// loading library
Napi::Boolean LoadLibraryWrapped(const Napi::CallbackInfo &info)
{
    Napi::Env env = info.Env();
    if (info.Length() < 1 || !info[0].IsString())
    {
        Napi::TypeError::New(env, "Engine path required").ThrowAsJavaScriptException();
        return Napi::Boolean::New(env, false);
    }
    std::string path = info[0].As<Napi::String>();

#ifdef _WIN32
    // UTF-8 → wchar_t conversion
    int wlen = MultiByteToWideChar(CP_UTF8, 0, path.c_str(), -1, NULL, 0);
    if (wlen <= 0)
    {
        Napi::TypeError::New(env, "Invalid engine path").ThrowAsJavaScriptException();
        return Napi::Boolean::New(env, false);
    }
    std::wstring wpath(wlen, 0);
    MultiByteToWideChar(CP_UTF8, 0, path.c_str(), -1, &wpath[0], wlen);
    int result = TSANPR_load(&tsanpr, wpath.c_str());
#else
    int result = TSANPR_load(&tsanpr, path.c_str());
#endif
    if (result != 0)
    {
        Napi::Error::New(env, "TSANPR_load failed: " + std::to_string(result)).ThrowAsJavaScriptException();
        return Napi::Boolean::New(env, false);
    }
    return Napi::Boolean::New(env, true);
}

// anpr_initialize
Napi::String AnprInitialize(const Napi::CallbackInfo &info)
{
    Napi::Env env = info.Env();
    if (!tsanpr.anpr_initialize)
    {
        Napi::Error::New(env, "anpr_initialize not loaded").ThrowAsJavaScriptException();
        return Napi::String::New(env, "");
    }
    std::string param = info[0].As<Napi::String>();
    const char *result = tsanpr.anpr_initialize(param.c_str());
    return Napi::String::New(env, result ? result : "");
}

// anpr_read_file
Napi::String AnprReadFile(const Napi::CallbackInfo &info)
{
    Napi::Env env = info.Env();
    if (!tsanpr.anpr_read_file)
    {
        Napi::Error::New(env, "anpr_read_file not loaded").ThrowAsJavaScriptException();
        return Napi::String::New(env, "");
    }
    std::string imgfile = info[0].As<Napi::String>();
    std::string outputFormat = info[1].As<Napi::String>();
    std::string options = info[2].As<Napi::String>();
    const char *result = tsanpr.anpr_read_file(imgfile.c_str(), outputFormat.c_str(), options.c_str());
    return Napi::String::New(env, result ? result : "");
}

// anpr_read_pixels
Napi::String AnprReadPixels(const Napi::CallbackInfo &info)
{
    Napi::Env env = info.Env();
    if (!tsanpr.anpr_read_pixels)
    {
        Napi::Error::New(env, "anpr_read_pixels not loaded").ThrowAsJavaScriptException();
        return Napi::String::New(env, "");
    }
    if (info.Length() < 7)
    {
        Napi::TypeError::New(env, "7 arguments required").ThrowAsJavaScriptException();
        return Napi::String::New(env, "");
    }
    Napi::Buffer<unsigned char> buf = info[0].As<Napi::Buffer<unsigned char>>();
    unsigned long width = info[1].As<Napi::Number>().Uint32Value();
    unsigned long height = info[2].As<Napi::Number>().Uint32Value();
    long stride = info[3].As<Napi::Number>().Int32Value();
    std::string pixelFormat = info[4].As<Napi::String>();
    std::string outputFormat = info[5].As<Napi::String>();
    std::string options = info[6].As<Napi::String>();
    const char *result = tsanpr.anpr_read_pixels(
        buf.Data(), width, height, stride,
        pixelFormat.c_str(), outputFormat.c_str(), options.c_str());
    return Napi::String::New(env, result ? result : "");
}

// module initialization
Napi::Object Init(Napi::Env env, Napi::Object exports)
{
    exports.Set("loadLibrary", Napi::Function::New(env, LoadLibraryWrapped));
    exports.Set("anpr_initialize", Napi::Function::New(env, AnprInitialize));
    exports.Set("anpr_read_file", Napi::Function::New(env, AnprReadFile));
    exports.Set("anpr_read_pixels", Napi::Function::New(env, AnprReadPixels));
    return exports;
}

NODE_API_MODULE(tsanpraddon, Init)
