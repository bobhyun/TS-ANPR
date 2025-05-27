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

#include <stdio.h>
#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include <opencv2/opencv.hpp>
#include "tsanpr.h"

TSANPR tsanpr;

static const std::string examplesBaseDir = "../..";

// Generate engine filename depending on platform
std::string getEngineFileName()
{
#ifdef _WIN32
#ifdef _WIN64
    // 64-bit Windows
    return examplesBaseDir + "\\bin\\windows-x86_64\\tsanpr.dll";
#else
    // 32-bit Windows
    return examplesBaseDir + "\\bin\\windows-x86\\tsanpr.dll";
#endif
#elif defined(__x86_64__) || defined(_M_X64)
    // 64-bit Linux
    return examplesBaseDir + "/bin/linux-x86_64/libtsanpr.so";
#elif defined(__aarch64__)
    // 64-bit ARM Linux
    return examplesBaseDir + "/bin/linux-aarch64/libtsanpr.so";
#else
    return "";
#endif
}

// Convert std::string to wchar_t* on Windows
#ifdef _WIN32
std::wstring to_wstring(const std::string &str)
{
    int len = MultiByteToWideChar(CP_UTF8, 0, str.c_str(), -1, nullptr, 0);
    std::wstring wstr(len, 0);
    MultiByteToWideChar(CP_UTF8, 0, str.c_str(), -1, &wstr[0], len);
    // Remove trailing null character
    if (!wstr.empty() && wstr.back() == L'\0')
        wstr.pop_back();
    return wstr;
}
#endif

void readImageFile(const char *imgfile, const char *outputFormat, const char *options)
{
    printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options);
    const char *result = tsanpr.anpr_read_file(imgfile, outputFormat, options);
    printf("%s\n", result);
}

void readEncodedImage(const char *imgfile, const char *outputFormat, const char *options)
{
    printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options);

    std::ifstream file(imgfile, std::ios::binary);
    if (!file)
    {
        printf("File open failed\n");
        return;
    }
    std::vector<unsigned char> encodedImg((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    file.close();

    const char *result = tsanpr.anpr_read_pixels(
        encodedImg.data(),
        (unsigned long)encodedImg.size(),
        0,
        0,
        "encoded",
        outputFormat,
        options);
    printf("%s\n", result);
}

const char *getPixelFormat(const cv::Mat &img)
{
    int channels = img.channels();
    if (channels == 1)
        return "GRAY";
    else if (channels == 2)
        return "BGR565";
    else if (channels == 3)
        return "BGR";
    else if (channels == 4)
        return "BGRA";
    return nullptr;
}

void readPixelBuffer(const char *imgfile, const char *outputFormat, const char *options)
{
    printf("%s (outputFormat=\"%s\", options=\"%s\") => ", imgfile, outputFormat, options);

    cv::Mat img = cv::imread(imgfile);
    if (img.empty())
    {
        perror("Image load failed!");
        return;
    }

    const char *pixelFormat = getPixelFormat(img);
    if (!pixelFormat)
    {
        perror("Unknown pixel format!");
        return;
    }

    const char *result = tsanpr.anpr_read_pixels(
        (const unsigned char *)img.data,
        img.cols,
        img.rows,
        (int)img.step,
        pixelFormat,
        outputFormat,
        options);
    printf("%s\n", result);
}

int readLicensePlates(const std::string &countryCode)
{
    // NOTICE:
    // anpr_initialize should be called only once after library load.
    // Therefore, it is not possible to change the country code after anpr_initialize has been called.
    // While using the free trial license, you can try all languages.
    // When you purchase a commercial license, you can only use the selected language.
    std::string initParams = "text;country=" + countryCode;
    const char *error = tsanpr.anpr_initialize(initParams.c_str());
    if (error && error[0])
    {
        printf("anpr_initialize() failed (error=%s)\n", error);
        return -1;
    }

    std::string imageDir = examplesBaseDir + "/img/" + countryCode + "/";

    // TODO: Try each function as needed
    void (*anprFunc)(const char *, const char *, const char *) = readImageFile;
    // void (*anprFunc)(const char *, const char *, const char *) = readEncodedImage;
    // void (*anprFunc)(const char *, const char *, const char *) = readPixelBuffer;

    // TODO: Try each output format as needed
    const char *outputFormat = "text";
    // const char* outputFormat = "json";
    // const char* outputFormat = "yaml";
    // const char* outputFormat = "xml";
    // const char* outputFormat = "csv";

    anprFunc((imageDir + "licensePlate.jpg").c_str(), outputFormat, ""); // Single license plate recognition (default)
    anprFunc((imageDir + "multiple.jpg").c_str(), outputFormat, "vm");   // Recognize multiple license plates attached to vehicles
    anprFunc((imageDir + "multiple.jpg").c_str(), outputFormat, "vmb");  // Recognize multiple license plates including motorcycles
    anprFunc((imageDir + "surround.jpg").c_str(), outputFormat, "vms");  // Recognize multiple license plates with surround detection
    anprFunc((imageDir + "surround.jpg").c_str(), outputFormat, "dms");  // Recognize multiple surrounding objects (vehicles)
    anprFunc((imageDir + "surround.jpg").c_str(), outputFormat, "dmsr"); // Recognize multiple surrounding objects and license plates

    // Recognize multiple surrounding objects and license plates within RoI
    anprFunc((imageDir + "surround.jpg").c_str(), outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700");

    return 0;
}

int main(int ac, char **av)
{
    std::string engineFileName = getEngineFileName();
    int res = 0;
#ifdef _WIN32
    std::wstring wEngineFileName = to_wstring(engineFileName);
    res = TSANPR_load(&tsanpr, wEngineFileName.c_str());
#else
    res = TSANPR_load(&tsanpr, engineFileName.c_str());
#endif
    if (res < 0)
        return res;

    // TODO: Try each country code as needed
    readLicensePlates("KR");
    // readLicensePlates("JP");
    // readLicensePlates("VN");

    TSANPR_unload();
    return 0;
}
