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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "com_example_anpr_TSANPR.h"
#include <jni.h>
#include <string>
#ifdef _WIN32
#include <windows.h>
#include <codecvt>
#include <locale>
#endif
#include "tsanpr.h"

static TSANPR g_tsanpr;

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Loads the engine DLL/SO with the given path from Java.
 * Signature: private native int nativeInit(String enginePath);
 */
JNIEXPORT jint JNICALL Java_com_example_anpr_TSANPR_nativeInit(JNIEnv *env, jobject, jstring enginePath) {
    if (enginePath == nullptr) return -100;
#ifdef _WIN32
    // Convert Java UTF-8 string to wchar_t*
    const char* utf8Path = env->GetStringUTFChars(enginePath, nullptr);
    if (!utf8Path) return -101;
    int wlen = MultiByteToWideChar(CP_UTF8, 0, utf8Path, -1, NULL, 0);
    std::wstring wpath(wlen, 0);
    MultiByteToWideChar(CP_UTF8, 0, utf8Path, -1, &wpath[0], wlen);
    env->ReleaseStringUTFChars(enginePath, utf8Path);
    // Remove trailing null character
    if (!wpath.empty() && wpath.back() == L'\0') wpath.pop_back();
    int ret = TSANPR_load(&g_tsanpr, wpath.c_str());
    return ret;
#else
    // Use UTF-8 path directly
    const char* cpath = env->GetStringUTFChars(enginePath, nullptr);
    int ret = TSANPR_load(&g_tsanpr, cpath);
    env->ReleaseStringUTFChars(enginePath, cpath);
    return ret;
#endif
}

/**
 * Unloads the engine DLL/SO.
 * Signature: private native void nativeRelease();
 */
JNIEXPORT void JNICALL Java_com_example_anpr_TSANPR_nativeRelease(JNIEnv *, jobject) {
    TSANPR_unload();
}

/**
 * Calls anpr_initialize from the loaded engine.
 */
JNIEXPORT jstring JNICALL Java_com_example_anpr_TSANPR_anpr_1initialize(JNIEnv *env, jobject, jstring mode) {
    if (!g_tsanpr.anpr_initialize) return env->NewStringUTF("Engine not loaded");
    const char* c_mode = env->GetStringUTFChars(mode, nullptr);
    const char* result = g_tsanpr.anpr_initialize(c_mode);
    env->ReleaseStringUTFChars(mode, c_mode);
    if (!result) return nullptr;
    return env->NewStringUTF(result);
}

/**
 * Calls anpr_read_file from the loaded engine.
 */
JNIEXPORT jstring JNICALL Java_com_example_anpr_TSANPR_anpr_1read_1file(JNIEnv *env, jobject, jstring imgFileName, jstring outputFormat, jstring options) {
    if (!g_tsanpr.anpr_read_file) return env->NewStringUTF("Engine not loaded");
    const char* c_img = env->GetStringUTFChars(imgFileName, nullptr);
    const char* c_fmt = env->GetStringUTFChars(outputFormat, nullptr);
    const char* c_opt = env->GetStringUTFChars(options, nullptr);
    const char* result = g_tsanpr.anpr_read_file(c_img, c_fmt, c_opt);
    env->ReleaseStringUTFChars(imgFileName, c_img);
    env->ReleaseStringUTFChars(outputFormat, c_fmt);
    env->ReleaseStringUTFChars(options, c_opt);
    if (!result) return nullptr;
    return env->NewStringUTF(result);
}

/**
 * Calls anpr_read_pixels from the loaded engine.
 */
JNIEXPORT jstring JNICALL Java_com_example_anpr_TSANPR_anpr_1read_1pixels(JNIEnv *env, jobject,
    jbyteArray pixels, jlong width, jlong height, jlong stride,
    jstring pixelFormat, jstring outputFormat, jstring options) {
    if (!g_tsanpr.anpr_read_pixels) return env->NewStringUTF("Engine not loaded");
    jsize len = env->GetArrayLength(pixels);
    jbyte* data = env->GetByteArrayElements(pixels, nullptr);
    const char* c_pixfmt = env->GetStringUTFChars(pixelFormat, nullptr);
    const char* c_outfmt = env->GetStringUTFChars(outputFormat, nullptr);
    const char* c_opt = env->GetStringUTFChars(options, nullptr);
    const char* result = g_tsanpr.anpr_read_pixels(
        (const unsigned char*)data,
        (unsigned long)width,
        (unsigned long)height,
        (long)stride,
        c_pixfmt,
        c_outfmt,
        c_opt
    );
    env->ReleaseByteArrayElements(pixels, data, JNI_ABORT);
    env->ReleaseStringUTFChars(pixelFormat, c_pixfmt);
    env->ReleaseStringUTFChars(outputFormat, c_outfmt);
    env->ReleaseStringUTFChars(options, c_opt);
    if (!result) return nullptr;
    return env->NewStringUTF(result);
}

#ifdef __cplusplus
}   // extern "C"
#endif